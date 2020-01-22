module Feature.User.User where

import Protolude
import Data.Aeson
import Infrastructure.Utils.Id
import Database.PostgreSQL.Simple
import Data.Time.Clock (UTCTime)

instance FromRow User
data User = User
  { userId        :: Id User
  , userUsername  :: Text
  , userEmail :: Text
  , userPassword  :: Text
  , userCreatedOn :: UTCTime
  } deriving Generic

instance ToJSON PublicUser
data PublicUser = PublicUser
  { username  :: Text
  , createdOn :: UTCTime
  } deriving Generic

toPublicUser :: User -> PublicUser
toPublicUser user =
  PublicUser { username = userUsername user, createdOn = userCreatedOn user }
