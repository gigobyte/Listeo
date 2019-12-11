module Feature.PlaylistTag.PlaylistTag where

import Protolude
import Infrastructure.Utils.Id (Id)
import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

instance ToJSON PublicPlaylistTag
data PublicPlaylistTag = PublicPlaylistTag
  { name :: Text
  } deriving Generic

data PlaylistTag = PlaylistTag
  { playlistTagId :: Id PlaylistTag
  , playlistTagName :: Text
  }

instance FromRow PlaylistTag where
  fromRow = PlaylistTag <$> field <*> field

toPublicPlaylistTag :: PlaylistTag -> PublicPlaylistTag
toPublicPlaylistTag dbTag = PublicPlaylistTag { name = playlistTagName dbTag }
