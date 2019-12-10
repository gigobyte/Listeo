module Feature.User.UserRepoClass where

import Protolude
import Feature.User.User (User)

data InsertUser = InsertUser
  { username :: Text
  , password :: Text
  }

class Monad m => UserRepo m where
  insertUser :: InsertUser -> m ()
  findUser :: Text -> m (Maybe User)
