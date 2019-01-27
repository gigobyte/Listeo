module Feature.Register.Service
  ( register
  , RegisterError
  , RegisterBody
  )
where

import Protolude
import Feature.User.DB (UserDTO(..))
import Feature.User.Service
import Control.Monad.Trans.Maybe
import Control.Monad.Except (liftEither)
import Infrastructure.IO
import Infrastructure.AppError
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

instance Aeson.FromJSON RegisterBody
data RegisterBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic

instance Aeson.ToJSON RegisterError where
  toJSON err = Aeson.genericToJSON (Aeson.defaultOptions { Aeson.tagSingleConstructors = True }) err
data RegisterError
    = UserAlreadyExists
    deriving Generic

register
  :: (UserRepo m, MonadCrypto m)
  => LByteString
  -> m (Either (AppError RegisterError) ())
register rawBody = runExceptT $ do
  body <- liftEither $ parseBody rawBody
  user <- liftEither $ mkUserDTO body
  ExceptT $ tryToInsertUser user

parseBody :: LByteString -> Either (AppError RegisterError) RegisterBody
parseBody body = maybeToRight InvalidRequest (Aeson.decode body)

tryToInsertUser
  :: (UserRepo m, MonadCrypto m)
  => UserDTO
  -> m (Either (AppError RegisterError) ())
tryToInsertUser user = runExceptT $ do
  userExists <- lift $ doesUserAlreadyExist (dtoUsername user)

  when userExists (throwE (DomainError UserAlreadyExists))

  maybeToExceptT ServerError $ do
    updatedUser <- MaybeT $ hashPasswordInUser user
    lift $ insertUser updatedUser

mkUserDTO :: RegisterBody -> Either (AppError RegisterError) UserDTO
mkUserDTO req =
  maybeToRight ValidationFailed
    $   UserDTO
    <$> (validateUsername $ username req)
    <*> (validatePassword $ password req)

validateUsername :: Text -> Maybe Text
validateUsername str
  | T.length str < 4 = Nothing
  | otherwise        = Just (T.strip str)

validatePassword :: Text -> Maybe Text
validatePassword str
  | T.length str < 6 = Nothing
  | otherwise        = Just (T.strip str)

hashPasswordInUser :: MonadCrypto m => UserDTO -> m (Maybe UserDTO)
hashPasswordInUser user@UserDTO { dtoPassword } = do
  hashedPassword <- cryptoHash $ encodeUtf8 $ dtoPassword
  return (setHashedPassword <$> decodeUtf8 <$> hashedPassword)
 where
  setHashedPassword :: Text -> UserDTO
  setHashedPassword p = user { dtoPassword = p }

