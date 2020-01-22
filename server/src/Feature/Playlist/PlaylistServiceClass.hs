module Feature.Playlist.PlaylistServiceClass where

import Protolude
import Infrastructure.Utils.Id
import Feature.Playlist.CreatePlaylist.CreatePlaylistResult
import Feature.Playlist.GetPlaylist.GetPlaylistResult
import Feature.Playlist.Playlist
import Feature.User.User

class Monad m => PlaylistService m where
  createPlaylist :: LByteString -> User -> m (Either CreatePlaylistError (Id Playlist))
  getPlaylist :: LByteString -> Maybe User -> m (Either GetPlaylistError GetPlaylistResponse)

