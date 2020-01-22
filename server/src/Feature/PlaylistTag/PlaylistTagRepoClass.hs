module Feature.PlaylistTag.PlaylistTagRepoClass where

import Protolude
import Feature.Playlist.Playlist (Playlist)
import Feature.PlaylistTag.PlaylistTag (PlaylistTag)
import Infrastructure.Utils.Id (Id)

data InsertPlaylistTag = InsertPlaylistTag
  { insertPlaylistTagName :: Text
  , insertPlaylistTagPlaylistId :: Id Playlist
  }

class Monad m => PlaylistTagRepo m where
  insertPlaylistTag :: InsertPlaylistTag -> m ()
  findPlaylistTagsByPlaylist :: Id Playlist -> m [PlaylistTag]
