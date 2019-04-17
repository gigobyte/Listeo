module Feature.Playlist.CreatePlaylist.CreatePlaylistError where

import Protolude
import Data.Aeson (ToJSON)

instance ToJSON CreatePlaylistError
data CreatePlaylistError
    = InvalidRequest
    | ValidationFailed
    deriving Generic

