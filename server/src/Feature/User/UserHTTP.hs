module Feature.User.UserHTTP
  ( routes
  )
where

import Protolude hiding (get)
import Feature.User.UserRepoClass (UserRepo)
import Web.Scotty.Trans (get, json, ScottyT)
import Feature.User.PublicUser
import qualified Feature.User.User as User
import qualified Feature.Auth as Auth

dbUserToPublicUser :: User.User -> PublicUser
dbUserToPublicUser user =
  PublicUser {username = User.username user, createdOn = User.createdOn user}

routes :: (UserRepo m, MonadIO m) => ScottyT LText m ()
routes = do
  get "/me" $ do
    user <- Auth.requireUser
    json $ dbUserToPublicUser $ user
