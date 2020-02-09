module Feature.User.UserRepoClass where

import Protolude
import Feature.User.User
import Infrastructure.Utils.Id

data InsertUser = InsertUser
  { insertUserUsername :: Text
  , insertUserEmail :: Text
  , insertUserPassword :: Text
  }

class Monad m => UserRepo m where
  insertUser :: InsertUser -> m ()
  deleteUser :: Id User -> m ()
  findUserByUsername :: Text -> m (Maybe User)
  findUserByEmail :: Text -> m (Maybe User)
