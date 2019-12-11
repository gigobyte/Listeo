module Feature.User.User where

import Protolude
import Infrastructure.Utils.Id (Id)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock (UTCTime)

data User = User
  { id        :: Id User
  , username  :: Text
  , password  :: Text
  , createdOn :: UTCTime
  }

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field
