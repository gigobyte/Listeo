module Feature.User.UserHTTP
  ( routes
  )
where

import Protolude hiding (get)
import Feature.Auth.AuthServiceClass (AuthService, requireUser)
import Web.Scotty.Trans (get, json, ScottyT)
import Feature.User.User



routes :: (AuthService m, MonadIO m) => ScottyT LText m ()
routes = do
  get "/me" $ do
    user <- requireUser
    json $ toPublicUser user
