module Feature.User.UserHTTP where

import Protolude hiding (get)
import Feature.Auth.AuthServiceClass
import Network.HTTP.Types.Status (ok200)
import Web.Scotty.Trans
import Feature.User.User

routes :: (AuthService m, MonadIO m) => ScottyT LText m ()
routes = do
  get "/me" $ do
    user <- requireUser
    json $ toPublicUser user

  post "/delete-me" $ do
    user <- requireUser
    lift $ deleteCurrentUser user
    status ok200
