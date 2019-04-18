module Feature.Playlist.GetPlaylist.GetPlaylistResponse where

import Protolude
import Data.Aeson (ToJSON)
import Infrastructure.Utils.Id (Id)
import Feature.Playlist.Playlist (Playlist, PlaylistStyle, PlaylistPrivacy)
import Feature.PlaylistTag.PlaylistTag (PlaylistTag)

import Data.Time.Clock (UTCTime)

instance ToJSON GetPlaylistResponse
data GetPlaylistResponse
    = GetPlaylistResponse
        { id :: Id Playlist
        , name :: Text
        , style :: PlaylistStyle
        , privacy :: PlaylistPrivacy
        , createdOn :: UTCTime
        , tags :: [PlaylistTag]
        }
    deriving Generic
