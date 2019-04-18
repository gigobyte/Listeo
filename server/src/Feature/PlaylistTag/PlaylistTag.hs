module Feature.PlaylistTag.PlaylistTag where

import Protolude
import Infrastructure.Utils.Id (Id)
import Database.MongoDB (Document, lookup, timestamp)
import Data.Time.Clock (UTCTime)
import Data.Aeson (ToJSON)

instance ToJSON PlaylistTag
data PlaylistTag = PlaylistTag
    { id :: Id PlaylistTag
    , name :: Text
    , createdOn :: UTCTime
    } deriving Generic

fromBson :: Document -> Maybe PlaylistTag
fromBson doc =
  PlaylistTag
    <$> lookup "_id"  doc
    <*> lookup "name" doc
    <*> (timestamp <$> lookup "_id" doc)
