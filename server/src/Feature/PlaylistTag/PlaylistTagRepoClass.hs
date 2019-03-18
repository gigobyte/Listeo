module Feature.PlaylistTag.PlaylistTagRepoClass where

import Protolude
import Feature.Playlist.Playlist (Playlist)
import Feature.PlaylistTag.PlaylistTagDTO (PlaylistTagDTO)
import Infrastructure.Utils.Id (Id)

class Monad m => PlaylistTagRepo m where
    insertPlaylistTag :: Id Playlist -> PlaylistTagDTO -> m ()

