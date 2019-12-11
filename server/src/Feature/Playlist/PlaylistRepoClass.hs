module Feature.Playlist.PlaylistRepoClass where

import Protolude
import Feature.Playlist.Playlist (Playlist, PlaylistStyle, PlaylistPrivacy)
import Infrastructure.Utils.Id (Id)

data InsertPlaylist = InsertPlaylist
  { insertPlaylistName :: Text
  , insertPlaylistDescription :: Text
  , insertPlaylistStyle :: PlaylistStyle
  , insertPlaylistPrivacy :: PlaylistPrivacy
  }

class Monad m => PlaylistRepo m where
  insertPlaylist :: InsertPlaylist -> m (Id Playlist)
  findPlaylist :: Text -> m (Maybe Playlist)
