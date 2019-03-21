module Feature.Playlist.PlaylistServiceClass where

import Protolude
import Infrastructure.AppError (GeneralAppError)
import Infrastructure.Utils.Id (Id)
import Feature.Playlist.Playlist (Playlist)

class Monad m => PlaylistService m where
    createPlaylist :: LByteString -> m (Either GeneralAppError (Id Playlist))


