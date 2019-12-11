module Feature.User.User where

import Protolude
import Data.Aeson
import Infrastructure.Utils.Id (Id)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data User = User
  { userId        :: Id User
  , userUsername  :: Text
  , userPassword  :: Text
  , userCreatedOn :: UTCTime
  }

instance ToJSON PublicUser
data PublicUser = PublicUser
  { username  :: Text
  , createdOn :: UTCTime
  } deriving Generic

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

toPublicUser :: User -> PublicUser
toPublicUser user =
  PublicUser { username = userUsername user, createdOn = userCreatedOn user }
