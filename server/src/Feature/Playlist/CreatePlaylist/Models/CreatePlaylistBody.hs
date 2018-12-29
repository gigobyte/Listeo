module Feature.Playlist.CreatePlaylist.Models.CreatePlaylistBody
  ( CreatePlaylistBody(..)
  )
where

import Protolude
import qualified Data.Aeson as Aeson

data PlaylistPrivacy = Public | Private
data PlaylistStyle = Unordered | Ranked

instance Aeson.FromJSON CreatePlaylistBody
data CreatePlaylistBody = CreatePlaylistBody
    { name :: Text
    , tags :: [Text]
    , description :: Text
    , privacy :: PlaylistPrivacy
    , style :: PlaylistStyle
    } deriving Generic
