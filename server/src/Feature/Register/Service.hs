module Feature.Register.Service
  ( register
  , RegisterError
  , RegisterBody
  )
where

import Protolude
import Feature.User.DB (User(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Except (liftEither)
import qualified Data.Bson as Bson
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time
import qualified Database.MongoDB as DB
import qualified Feature.User.DB as DB
import qualified Feature.User.DB as User
import qualified Infrastructure.Crypto as Crypto

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

register :: DB.Pipe -> LByteString -> IO (Either RegisterError ())
register pipe rawBody = runExceptT $ do
  dateNow <- liftIO Time.getCurrentTime
  body    <- liftEither $ parseBody rawBody
  user    <- liftEither $ mkUser dateNow body

  ExceptT $ insertUser pipe user

parseBody :: LByteString -> Either RegisterError RegisterBody
parseBody body = maybeToRight ValidationFailed (Aeson.decode body)

insertUser :: DB.Pipe -> User -> IO (Either RegisterError ())
insertUser pipe user = runExceptT $ do
  userExists <- liftIO $ DB.doesUserAlreadyExist pipe user

  when userExists (throwError UserAlreadyExists)

  maybeToExceptT ServerError $ do
    updatedUser <- MaybeT $ hashPasswordInUser user
    liftIO $ DB.insertUser pipe updatedUser

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

hashPasswordInUser :: User -> IO (Maybe User)
hashPasswordInUser user@User { password } = do
  hashedPassword <- Crypto.hash $ encodeUtf8 $ password
  return (setHashedPassword <$> decodeUtf8 <$> hashedPassword)
 where
  setHashedPassword :: Text -> User
  setHashedPassword p = user { User.password = p }
