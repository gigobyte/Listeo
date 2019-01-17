module Feature.User.HTTP
  ( me
  )
where

import Protolude
import qualified Database.MongoDB as DB
import qualified Web.Scotty as Scotty
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time
import qualified Feature.User.DB as User

instance Aeson.ToJSON PublicUser
data PublicUser = PublicUser
    { username  :: Text
    , createdOn :: Time.UTCTime
    } deriving Generic

dbUserToPublicUser :: User.User -> PublicUser
dbUserToPublicUser user =
  PublicUser {username = User.username user, createdOn = User.createdOn user}

me :: DB.Pipe -> User.User -> Scotty.ActionM ()
me _ user = do
  Scotty.json $ dbUserToPublicUser $ user
