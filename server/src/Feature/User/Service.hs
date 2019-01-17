module Feature.User.Service where

import Protolude
import Feature.User.DB (User)

class Monad m => UserRepo m where
    insertUser :: User -> m ()
    doesUserAlreadyExist :: User -> m Bool
    findUser :: Text -> m (Maybe User)
