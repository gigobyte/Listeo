module Feature.Register.Service
  ( register
  , RegisterError
  , RegisterBody
  )
where

import Protolude
import Feature.User.DB (User(..))
import Feature.User.Service
import Control.Monad.Trans.Maybe
import Control.Monad.Except (liftEither)
import Infrastructure.Types
import qualified Data.Bson as Bson
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time
import qualified Feature.User.DB as User

instance Aeson.FromJSON RegisterBody
data RegisterBody = RegisterBody
    { registerBodyUsername :: Text
    , registerBodyPassword :: Text
    } deriving Generic

instance Aeson.ToJSON RegisterError
data RegisterError
    = ValidationFailed
    | UserAlreadyExists
    | ServerError
    deriving Generic

register
  :: (UserRepo m, MonadTime m, MonadCrypto m)
  => LByteString
  -> m (Either RegisterError ())
register rawBody = do
  dateNow <- currentTime

  runExceptT $ do
    body <- liftEither $ parseBody rawBody
    user <- liftEither $ mkUser dateNow body
    ExceptT $ tryToInsertUser user

parseBody :: LByteString -> Either RegisterError RegisterBody
parseBody body = maybeToRight ValidationFailed (Aeson.decode body)

tryToInsertUser
  :: (UserRepo m, MonadCrypto m) => User -> m (Either RegisterError ())
tryToInsertUser user = runExceptT $ do
  userExists <- lift $ doesUserAlreadyExist user

  when userExists (throwError UserAlreadyExists)

  maybeToExceptT ServerError $ do
    updatedUser <- MaybeT $ hashPasswordInUser user
    lift $ insertUser updatedUser

mkUser :: Time.UTCTime -> RegisterBody -> Either RegisterError User
mkUser dateNow req =
  maybeToRight ValidationFailed
    $   User
    <$> (return $ Bson.Oid 0 0)
    <*> (validateUsername $ registerBodyUsername req)
    <*> (validatePassword $ registerBodyPassword req)
    <*> return dateNow

validateUsername :: Text -> Maybe Text
validateUsername str
  | T.length str < 4 = Nothing
  | otherwise        = Just (T.strip str)

validatePassword :: Text -> Maybe Text
validatePassword str
  | T.length str < 6 = Nothing
  | otherwise        = Just (T.strip str)

hashPasswordInUser :: (MonadCrypto m) => User -> m (Maybe User)
hashPasswordInUser user@User { password } = do
  hashedPassword <- cryptoHash $ encodeUtf8 $ password
  return (setHashedPassword <$> decodeUtf8 <$> hashedPassword)
 where
  setHashedPassword :: Text -> User
  setHashedPassword p = user { User.password = p }

