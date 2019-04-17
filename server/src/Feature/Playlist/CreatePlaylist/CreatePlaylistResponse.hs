module Feature.Playlist.CreatePlaylist.CreatePlaylistResponse where

import Protolude
import Data.Aeson (ToJSON)

instance ToJSON CreatePlaylistResponse
data CreatePlaylistResponse
    = CreatePlaylistResponse { playlistId :: Text }
    deriving Generic

