module Feature.Register.RegisterService where

import Protolude hiding (hash)
import Data.Aeson
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Infrastructure.MonadCrypto
import Feature.Login.LoginService
import Feature.Register.RegisterResult
import Feature.User.UserRepoClass
import qualified Data.Text as T

data Register = Register
  { registerUsername :: Text
  , registerEmail :: Text
  , registerPassword :: Text
  }

instance FromJSON Register where
  parseJSON = withObject "register"
    $ \o -> Register <$> o .: "username" <*> o .: "email" <*> o .: "password"

register
  :: (UserRepo m, MonadCrypto m) => LByteString -> m (Either RegisterError Text)
register rawBody = runExceptT $ do
  body <- liftEither $ parseBody rawBody
  user <- liftEither $ mkInsertUser body
  ExceptT $ tryToInsertUser user
  return $ generateJwtToken (insertUserUsername user)

parseBody :: LByteString -> Either RegisterError Register
parseBody body = maybeToRight InvalidRequest (decode body)

tryToInsertUser
  :: (UserRepo m, MonadCrypto m) => InsertUser -> m (Either RegisterError ())
tryToInsertUser user = runExceptT $ do
  userExists <- lift $ doesUserAlreadyExist user

  when userExists $ throwE UserAlreadyExists

  maybeToExceptT PasswordHashingFailed $ do
    updatedUser <- MaybeT $ hashPasswordInUser user
    lift $ insertUser updatedUser

doesUserAlreadyExist :: (UserRepo m) => InsertUser -> m Bool
doesUserAlreadyExist user = do
  userWithSameName  <- findUserByUsername (insertUserUsername user)
  userWithSameEmail <- findUserByEmail (insertUserEmail user)
  return $ isJust (userWithSameName <|> userWithSameEmail)

mkInsertUser :: Register -> Either RegisterError InsertUser
mkInsertUser req =
  maybeToRight ValidationFailed
    $   InsertUser
    <$> (validateUsername $ registerUsername req)
    <*> (validateEmail $ registerEmail req)
    <*> (validatePassword $ registerPassword req)

validateUsername :: Text -> Maybe Text
validateUsername str
  | T.length str < 4  = Nothing
  | T.length str > 99 = Nothing
  | otherwise         = Just str

validateEmail :: Text -> Maybe Text
validateEmail str
  | not $ T.isInfixOf "@" str = Nothing
  | otherwise                 = Just str

validatePassword :: Text -> Maybe Text
validatePassword str
  | T.length str < 6 = Nothing
  | otherwise        = Just str

hashPasswordInUser :: MonadCrypto m => InsertUser -> m (Maybe InsertUser)
hashPasswordInUser user@InsertUser { insertUserPassword } = do
  hashedPassword <- hash $ encodeUtf8 $ insertUserPassword
  return (setHashedPassword . decodeUtf8 <$> hashedPassword)
 where
  setHashedPassword :: Text -> InsertUser
  setHashedPassword p = user { insertUserPassword = p }

