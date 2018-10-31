module Feature.User.Service where

import qualified Data.Time.Clock        as Time
import           Database.MongoDB       (Action, Value)
import           Feature.User.DB        as DB
import           Feature.User.Types     (RegisterBody (..), RegisterError (..))
import qualified Feature.User.UserModel as User
import           Infrastructure.Maybe   (maybeToEither)
import           Protolude              hiding (maybeToEither)

insertUser :: User.User -> Action IO (Either RegisterError Value)
insertUser user = do
    userExists <- DB.doesUserExist $ User.unwrapUsername $ User.username user

    if not userExists then
        Right <$> DB.insertUser user
    else
        pure $ Left UserAlreadyExists

mkUser :: Time.UTCTime -> RegisterBody -> Either RegisterError User.User
mkUser dateNow req =
    maybeToEither ValidationFailed $ User.User
        <$> (User.mkUsername $ username req)
        <*> (User.mkPassword $ password req)
        <*> pure dateNow
