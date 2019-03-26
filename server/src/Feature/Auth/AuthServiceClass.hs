module Feature.Auth.AuthServiceClass where

import Protolude
import Feature.User.User (User)
import Web.Scotty.Trans (ActionT)

class Monad m => AuthService m where
    requireUser :: ActionT LText m User

