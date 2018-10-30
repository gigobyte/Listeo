module Feature.User.Service where

import           Database.MongoDB       (Action, Value)
import           Feature.User.DB        as DB
import           Feature.User.Types     (RegisterError (..))
import qualified Feature.User.UserModel as User
import           Protolude

insertUser :: User.User -> Action IO (Either RegisterError Value)
insertUser user = do
    userExists <- doesUserExist $ User.unwrapUsername $ User.username user

    if userExists then do
        doc <- DB.insertUser user
        pure $ Right doc
    else
        pure $ Left UserAlreadyExists
