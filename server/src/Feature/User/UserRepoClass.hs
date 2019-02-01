module Feature.User.UserRepoClass where

import Protolude
import Feature.User.User (User)
import Feature.User.UserDTO (UserDTO)

class Monad m => UserRepo m where
  insertUser :: UserDTO -> m ()
  findUser :: Text -> m (Maybe User)
