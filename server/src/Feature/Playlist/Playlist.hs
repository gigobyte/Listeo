module Feature.Playlist.Playlist where

import Protolude
import Infrastructure.Utils.Id (Id)
import Database.MongoDB (Document, lookup)
import Data.Aeson (FromJSON)
import Data.Time.Clock (UTCTime)

instance FromJSON PlaylistPrivacy
data PlaylistPrivacy
    = Public
    | Private
    deriving (Generic, Enum)

instance FromJSON PlaylistStyle
data PlaylistStyle
    = Unordered
    | Ranked
    deriving (Generic, Enum)

data Playlist = Playlist
    { id :: Id Playlist
    , name :: Text
    , style :: PlaylistStyle
    , privacy :: PlaylistPrivacy
    , createdOn :: UTCTime
    }

fromBson :: Document -> Maybe Playlist
fromBson doc =
  Playlist
    <$> lookup "_id"  doc
    <*> lookup "name" doc
    <*> (toEnum <$> lookup "style" doc)
    <*> (toEnum <$> lookup "privacy" doc)
    <*> lookup "createdOn" doc
