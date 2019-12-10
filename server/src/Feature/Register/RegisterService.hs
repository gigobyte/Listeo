module Feature.Register.RegisterService
  ( register
  )
where

import Protolude hiding (hash)
import Control.Monad.Trans.Maybe
import Control.Monad.Except (liftEither)
import Infrastructure.MonadCrypto
import Feature.Login.LoginService (generateJwtToken)
import Feature.Register.RegisterResult (RegisterError(..))
import Feature.User.UserRepoClass (InsertUser(..), UserRepo(..))
import qualified Data.Text as T
import Data.Aeson

data Register = Register
  { registerUsername :: Text
  , registerPassword :: Text
  }

instance FromJSON Register where
  parseJSON = withObject "register"
    $ \o -> Register <$> o .: "username" <*> o .: "password"

register
  :: (UserRepo m, MonadCrypto m) => LByteString -> m (Either RegisterError Text)
register rawBody = runExceptT $ do
  body <- liftEither $ parseBody rawBody
  user <- liftEither $ mkInsertUser body
  ExceptT $ tryToInsertUser user
  return $ generateJwtToken (username user)

parseBody :: LByteString -> Either RegisterError Register
parseBody body = maybeToRight InvalidRequest (decode body)

tryToInsertUser
  :: (UserRepo m, MonadCrypto m) => InsertUser -> m (Either RegisterError ())
tryToInsertUser user = runExceptT $ do
  userExists <- lift $ doesUserAlreadyExist (username user)

  when userExists $ throwE UserAlreadyExists

  maybeToExceptT PasswordHashingFailed $ do
    updatedUser <- MaybeT $ hashPasswordInUser user
    lift $ insertUser updatedUser

doesUserAlreadyExist :: (UserRepo m) => Text -> m Bool
doesUserAlreadyExist username = do
  userInDB <- findUser username
  return $ isJust userInDB

mkInsertUser :: Register -> Either RegisterError InsertUser
mkInsertUser req =
  maybeToRight ValidationFailed
    $   InsertUser
    <$> (validateUsername $ registerUsername req)
    <*> (validatePassword $ registerPassword req)

validateUsername :: Text -> Maybe Text
validateUsername str
  | T.length str < 4 = Nothing
  | otherwise        = Just (T.strip str)

validatePassword :: Text -> Maybe Text
validatePassword str
  | T.length str < 6 = Nothing
  | otherwise        = Just (T.strip str)

hashPasswordInUser :: MonadCrypto m => InsertUser -> m (Maybe InsertUser)
hashPasswordInUser user@InsertUser { password } = do
  hashedPassword <- hash $ encodeUtf8 $ password
  return (setHashedPassword . decodeUtf8 <$> hashedPassword)
 where
  setHashedPassword :: Text -> InsertUser
  setHashedPassword p = user { password = p }

