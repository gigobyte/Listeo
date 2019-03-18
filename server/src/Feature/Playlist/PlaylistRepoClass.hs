module Feature.Playlist.PlaylistRepoClass where

import Protolude
import Feature.Playlist.Playlist (Playlist)
import Feature.Playlist.PlaylistDTO (PlaylistDTO)
import Infrastructure.Utils.Id (Id)

class Monad m => PlaylistRepo m where
    insertPlaylist :: PlaylistDTO -> m (Id Playlist)
