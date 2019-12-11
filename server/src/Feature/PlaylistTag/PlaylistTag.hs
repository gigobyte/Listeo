module Feature.PlaylistTag.PlaylistTag where

import Protolude
import Infrastructure.Utils.Id (Id)
import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

instance ToJSON PlaylistTag
data PlaylistTag = PlaylistTag
  { id :: Id PlaylistTag
  , name :: Text
  } deriving Generic

instance FromRow PlaylistTag where
  fromRow = PlaylistTag <$> field <*> field
