module Feature.Register.Service
  ( insertUser
  , mkUser
  )
where

import Data.Bson (ObjectId(..))
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import Database.MongoDB (Action)
import Feature.Register.Types (RegisterBody(..), RegisterError(..))
import qualified Feature.User.DB as DB
import qualified Feature.User.Types as User
import qualified Infrastructure.Crypto as Crypto
import Protolude

insertUser :: User.User -> Action IO (Either RegisterError ())
insertUser user = do
  userInDB <- DB.findUser $ User.username (user :: User.User)

  case userInDB of
    Just _  -> pure $ Left UserAlreadyExists
    Nothing -> do
      updatedUser <- liftIO $ hashPasswordInUser user
      sequence $ maybeToRight ServerError $ DB.insertUser <$> updatedUser

mkUser :: Time.UTCTime -> RegisterBody -> Either RegisterError User.User
mkUser dateNow req =
  maybeToRight ValidationFailed
    $   User.User
    <$> (pure $ Oid 0 0)
    <*> (validateUsername $ username req)
    <*> (validatePassword $ password req)
    <*> pure dateNow

validateUsername :: Text -> Maybe Text
validateUsername str
  | T.length str < 4 = Nothing
  | otherwise        = Just $ T.strip str

validatePassword :: Text -> Maybe Text
validatePassword str
  | T.length str < 6 = Nothing
  | otherwise        = Just $ T.strip str

hashPasswordInUser :: User.User -> IO (Maybe User.User)
hashPasswordInUser user = do
  hashedPassword <- Crypto.hash $ encodeUtf8 $ User.password $ user
  pure $ (\p -> user { User.password = p }) <$> decodeUtf8 <$> hashedPassword
