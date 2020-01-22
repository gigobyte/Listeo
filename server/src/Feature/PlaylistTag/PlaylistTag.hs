module Feature.PlaylistTag.PlaylistTag where

import Protolude
import Infrastructure.Utils.Id
import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple

instance ToJSON PublicPlaylistTag
data PublicPlaylistTag = PublicPlaylistTag
  { name :: Text
  } deriving Generic

instance FromRow PlaylistTag
data PlaylistTag = PlaylistTag
  { playlistTagId :: Id PlaylistTag
  , playlistTagName :: Text
  } deriving Generic

toPublicPlaylistTag :: PlaylistTag -> PublicPlaylistTag
toPublicPlaylistTag dbTag = PublicPlaylistTag { name = playlistTagName dbTag }
