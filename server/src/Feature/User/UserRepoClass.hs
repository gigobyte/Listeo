module Feature.User.UserRepoClass where

import Protolude
import Feature.User.User (User)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

data InsertUser = InsertUser
  { insertUserUsername :: Text
  , insertUserPassword :: Text
  }

instance ToRow InsertUser where
  toRow x = [toField (insertUserUsername x), toField (insertUserPassword x)]

class Monad m => UserRepo m where
  insertUser :: InsertUser -> m ()
  findUser :: Text -> m (Maybe User)
