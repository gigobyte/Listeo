module Feature.User.UserHTTP
  ( routes
  )
where

import Protolude hiding (get)
import Feature.Auth.AuthServiceClass (AuthService, requireUser)
import Web.Scotty.Trans (get, json, ScottyT)
import Feature.User.PublicUser
import qualified Feature.User.User as User

dbUserToPublicUser :: User.User -> PublicUser
dbUserToPublicUser user =
  PublicUser {username = User.username user, createdOn = User.createdOn user}

routes :: (AuthService m, MonadIO m) => ScottyT LText m ()
routes = do
  get "/me" $ do
    user <- requireUser
    json $ dbUserToPublicUser $ user
