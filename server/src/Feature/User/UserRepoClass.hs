module Feature.User.UserRepoClass where

import Protolude
import Feature.User.User (User)

data InsertUser = InsertUser
  { insertUserUsername :: Text
  , insertUserPassword :: Text
  }

class Monad m => UserRepo m where
  insertUser :: InsertUser -> m ()
  findUser :: Text -> m (Maybe User)
