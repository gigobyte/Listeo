module Feature.Playlist.PlaylistRepoClass where

import Protolude
import Feature.Playlist.Playlist (Playlist, PlaylistStyle, PlaylistPrivacy)
import Feature.User.User (User)
import Infrastructure.Utils.Id (Id)

data InsertPlaylist = InsertPlaylist
  { insertPlaylistName :: Text
  , insertPlaylistDescription :: Text
  , insertPlaylistStyle :: PlaylistStyle
  , insertPlaylistPrivacy :: PlaylistPrivacy
  , insertPlaylistAuthorId :: Id User
  }

class Monad m => PlaylistRepo m where
  insertPlaylist :: InsertPlaylist -> m (Id Playlist)
  findPlaylist :: Id Playlist -> m (Maybe Playlist)
