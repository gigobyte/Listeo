module Feature.Playlist.PlaylistServiceClass where

import Protolude
import Infrastructure.Utils.Id (Id)
import Feature.Playlist.CreatePlaylist.CreatePlaylistError (CreatePlaylistError)
import Feature.Playlist.GetPlaylist.GetPlaylistError (GetPlaylistError)
import Feature.Playlist.GetPlaylist.GetPlaylistResponse (GetPlaylistResponse)
import Feature.Playlist.Playlist (Playlist)

class Monad m => PlaylistService m where
    createPlaylist :: LByteString -> m (Either CreatePlaylistError (Id Playlist))
    getPlaylist :: LByteString -> m (Either GetPlaylistError GetPlaylistResponse)

