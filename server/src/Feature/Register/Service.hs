module Feature.Register.Service
    ( insertUser
    , mkUser
    ) where

import qualified Data.Text              as T
import qualified Data.Time.Clock        as Time
import           Database.MongoDB       (Action, Value)
import           Feature.Register.Types (RegisterBody (..), RegisterError (..))
import qualified Feature.User.DB        as DB
import qualified Feature.User.Types     as User
import qualified Infrastructure.Crypto  as Crypto
import           Infrastructure.Maybe   (maybeToEither)
import           Protolude              hiding (maybeToEither)

insertUser :: User.User -> Action IO (Either RegisterError Value)
insertUser user = do
    userExists <- DB.doesUserExist $ User.username user

    if not userExists then do
        updatedUser <- liftIO $ hashPasswordInUser user

        case updatedUser of
            Just u  -> Right <$> DB.insertUser u
            Nothing -> pure $ Left ServerError
    else
        pure $ Left UserAlreadyExists

mkUser :: Time.UTCTime -> RegisterBody -> Either RegisterError User.User
mkUser dateNow req =
    maybeToEither ValidationFailed $ User.User
        <$> pure 0
        <*> (validateUsername $ username req)
        <*> (validatePassword $ password req)
        <*> pure dateNow

validateUsername :: Text -> Maybe Text
validateUsername str
    | T.length str < 4 = Nothing
    | otherwise = Just $ T.strip str

validatePassword :: Text -> Maybe Text
validatePassword str
    | T.length str < 6 = Nothing
    | otherwise = Just $ T.strip str

hashPasswordInUser :: User.User -> IO (Maybe User.User)
hashPasswordInUser user = do
    hashedPassword <- Crypto.hash $ encodeUtf8 $ User.password $ user

    pure $ (\p -> user { User.password = p }) <$> decodeUtf8 <$> hashedPassword
