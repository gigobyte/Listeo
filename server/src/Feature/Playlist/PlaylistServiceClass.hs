module Feature.Playlist.PlaylistServiceClass where

import Protolude
import Infrastructure.Utils.Id (Id)
import Feature.Playlist.CreatePlaylist.CreatePlaylistResult
  (CreatePlaylistError)
import Feature.Playlist.GetPlaylist.GetPlaylistResult
  (GetPlaylistError, GetPlaylistResponse)
import Feature.Playlist.Playlist (Playlist)
import Feature.User.User (User)

class Monad m => PlaylistService m where
  createPlaylist :: LByteString -> User -> m (Either CreatePlaylistError (Id Playlist))
  getPlaylist :: LByteString -> Maybe User -> m (Either GetPlaylistError GetPlaylistResponse)

