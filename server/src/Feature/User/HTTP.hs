module Feature.User.HTTP
  ( routes
  )
where

import Protolude hiding (get)
import Feature.User.Service (UserRepo)
import Web.Scotty.Trans (get, json, ScottyT)
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time
import qualified Feature.User.DB as User
import qualified Feature.Auth.HTTP as Auth

instance Aeson.ToJSON PublicUser
data PublicUser = PublicUser
  { username  :: Text
  , createdOn :: Time.UTCTime
  } deriving Generic

dbUserToPublicUser :: User.User -> PublicUser
dbUserToPublicUser user =
  PublicUser {username = User.username user, createdOn = User.createdOn user}

routes :: (UserRepo m, MonadIO m) => ScottyT LText m ()
routes = do
  get "/me" $ do
    user <- Auth.requireUser
    json $ dbUserToPublicUser $ user
