module Feature.Auth.AuthServiceClass where

import Protolude
import Feature.User.User (User)
import Web.Scotty.Trans

class Monad m => AuthService m where
  requireUser :: ActionT LText m User
  optionalUser :: ActionT LText m (Maybe User)
  deleteCurrentUser :: User -> m ()
