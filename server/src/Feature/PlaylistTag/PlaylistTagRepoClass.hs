module Feature.PlaylistTag.PlaylistTagRepoClass where

import Protolude
import Feature.Playlist.Playlist (Playlist)
import Feature.PlaylistTag.PlaylistTag (PlaylistTag)
import Infrastructure.Utils.Id (Id)

data InsertPlaylistTag = InsertPlaylistTag
  { insertPlaylistTagName :: Text
  }

class Monad m => PlaylistTagRepo m where
  insertPlaylistTag :: Id Playlist -> InsertPlaylistTag -> m ()
  findPlaylistTagsByPlaylist :: Text -> m [PlaylistTag]
