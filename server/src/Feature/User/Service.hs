module Feature.User.Service where

import Protolude
import Feature.User.DB (User, UserDTO)

class Monad m => UserRepo m where
  insertUser :: UserDTO -> m ()
  doesUserAlreadyExist :: Text -> m Bool
  findUser :: Text -> m (Maybe User)
