module Feature.Playlist.CreatePlaylist.CreatePlaylistBody where

import Protolude
import Data.Aeson (FromJSON)
import Feature.Playlist.Playlist (PlaylistStyle, PlaylistPrivacy)

instance FromJSON CreatePlaylistBody
data CreatePlaylistBody = CreatePlaylistBody
    { name :: Text
    , tags :: [Text]
    , privacy :: PlaylistPrivacy
    , style :: PlaylistStyle
    } deriving Generic
