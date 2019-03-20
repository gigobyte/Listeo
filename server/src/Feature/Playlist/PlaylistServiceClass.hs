module Feature.Playlist.PlaylistServiceClass where

import Protolude
import Infrastructure.AppError (GeneralAppError)

class Monad m => PlaylistService m where
    createPlaylist :: LByteString -> m (Either GeneralAppError ())


