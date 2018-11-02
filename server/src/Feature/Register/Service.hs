module Feature.Register.Service
    ( insertUser
    , mkUser
    ) where

import qualified Data.Time.Clock        as Time
import           Database.MongoDB       (Action, Value)
import qualified Feature.Register.DB    as DB
import           Feature.Register.Types (RegisterBody (..), RegisterError (..))
import qualified Feature.User.UserModel as User
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
        <$> (User.mkUsername $ username req)
        <*> (User.mkPassword $ password req)
        <*> pure dateNow

hashPasswordInUser :: User.User -> IO (Maybe User.User)
hashPasswordInUser user = do
    hashedPassword <- User.hashPassword $ User.password $ user

    pure $ (\p -> user { User.password = p }) <$> hashedPassword
