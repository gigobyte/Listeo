module Feature.Register.Service
  ( register
  , RegisterError
  , RegisterBody
  )
where

import Protolude
import Feature.User.DB (User(User))
import Feature.User.Service
import Control.Monad.Trans.Maybe
import Control.Monad.Except (liftEither)
import Infrastructure.Types
import Infrastructure.AppError
import qualified Data.Bson as Bson
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time
import qualified Feature.User.DB as User

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
  :: (UserRepo m, MonadTime m, MonadCrypto m)
  => LByteString
  -> m (Either (AppError RegisterError) ())
register rawBody = do
  dateNow <- currentTime

  runExceptT $ do
    body <- liftEither $ parseBody rawBody
    user <- liftEither $ mkUser dateNow body
    ExceptT $ tryToInsertUser user

parseBody :: LByteString -> Either (AppError RegisterError) RegisterBody
parseBody body = maybeToRight InvalidRequest (Aeson.decode body)

tryToInsertUser
  :: (UserRepo m, MonadCrypto m)
  => User
  -> m (Either (AppError RegisterError) ())
tryToInsertUser user = runExceptT $ do
  userExists <- lift $ doesUserAlreadyExist user

  when userExists (throwError (DomainError UserAlreadyExists))

  maybeToExceptT ServerError $ do
    updatedUser <- MaybeT $ hashPasswordInUser user
    lift $ insertUser updatedUser

mkUser :: Time.UTCTime -> RegisterBody -> Either (AppError RegisterError) User
mkUser dateNow req =
  maybeToRight ValidationFailed
    $   User
    <$> (return $ Bson.Oid 0 0)
    <*> (validateUsername $ username req)
    <*> (validatePassword $ password req)
    <*> return dateNow

validateUsername :: Text -> Maybe Text
validateUsername str
  | T.length str < 4 = Nothing
  | otherwise        = Just (T.strip str)

validatePassword :: Text -> Maybe Text
validatePassword str
  | T.length str < 6 = Nothing
  | otherwise        = Just (T.strip str)

hashPasswordInUser :: MonadCrypto m => User -> m (Maybe User)
hashPasswordInUser user@User { password } = do
  hashedPassword <- cryptoHash $ encodeUtf8 $ password
  return (setHashedPassword <$> decodeUtf8 <$> hashedPassword)
 where
  setHashedPassword :: Text -> User
  setHashedPassword p = user { User.password = p }

