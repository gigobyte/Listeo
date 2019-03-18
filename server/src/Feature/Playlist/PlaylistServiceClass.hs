module Feature.Playlist.PlaylistServiceClass where

import Protolude
import Infrastructure.AppError (AppError)

class Monad m => PlaylistService m where
    createPlaylist :: LByteString -> m (Either (AppError ()) ())


