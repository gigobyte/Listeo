module Feature.Register.RegisterService
  ( register
  )
where

import Protolude hiding (hash)
import Feature.User.UserDTO (UserDTO(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Except (liftEither)
import Infrastructure.MonadCrypto
import Feature.Register.RegisterError (RegisterError(..))
import Feature.Register.RegisterBody (RegisterBody)
import Feature.Login.LoginService (generateJwtToken)
import qualified Feature.Register.RegisterBody as RegisterBody
import Feature.User.UserRepoClass (UserRepo(..))
import qualified Feature.User.UserDTO as UserDTO
import qualified Data.Text as T
import Data.Aeson (decode)

register
  :: (UserRepo m, MonadCrypto m) => LByteString -> m (Either RegisterError Text)
register rawBody = runExceptT $ do
  body <- liftEither $ parseBody rawBody
  user <- liftEither $ mkUserDTO body
  ExceptT $ tryToInsertUser user
  return $ generateJwtToken (UserDTO.username user)

parseBody :: LByteString -> Either RegisterError RegisterBody
parseBody body = maybeToRight InvalidRequest (decode body)

tryToInsertUser
  :: (UserRepo m, MonadCrypto m) => UserDTO -> m (Either RegisterError ())
tryToInsertUser user = runExceptT $ do
  userExists <- lift $ doesUserAlreadyExist (UserDTO.username user)

  when userExists $ throwE UserAlreadyExists

  maybeToExceptT PasswordHashingFailed $ do
    updatedUser <- MaybeT $ hashPasswordInUser user
    lift $ insertUser updatedUser

doesUserAlreadyExist :: (UserRepo m) => Text -> m Bool
doesUserAlreadyExist username = do
  userInDB <- findUser username
  return $ isJust userInDB

mkUserDTO :: RegisterBody -> Either RegisterError UserDTO
mkUserDTO req =
  maybeToRight ValidationFailed
    $   UserDTO
    <$> (validateUsername $ RegisterBody.username req)
    <*> (validatePassword $ RegisterBody.password req)

validateUsername :: Text -> Maybe Text
validateUsername str
  | T.length str < 4 = Nothing
  | otherwise        = Just (T.strip str)

validatePassword :: Text -> Maybe Text
validatePassword str
  | T.length str < 6 = Nothing
  | otherwise        = Just (T.strip str)

hashPasswordInUser :: MonadCrypto m => UserDTO -> m (Maybe UserDTO)
hashPasswordInUser user@UserDTO { UserDTO.password } = do
  hashedPassword <- hash $ encodeUtf8 $ password
  return (setHashedPassword <$> decodeUtf8 <$> hashedPassword)
 where
  setHashedPassword :: Text -> UserDTO
  setHashedPassword p = user { UserDTO.password = p }

