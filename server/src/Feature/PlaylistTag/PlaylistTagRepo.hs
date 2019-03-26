module Feature.PlaylistTag.PlaylistTagRepo where

import Protolude
import Data.Maybe (fromJust)
import Feature.Playlist.Playlist (Playlist)
import Feature.PlaylistTag.PlaylistTag (PlaylistTag)
import Feature.PlaylistTag.PlaylistTagDTO (PlaylistTagDTO(..))
import qualified Feature.PlaylistTag.PlaylistTagDTO as PlaylistTagDTO
import Infrastructure.DB (MonadDB, runQuery, withConn)
import Infrastructure.Utils.Id (Id)
import Database.MongoDB (insert, insert_, cast', (=:))

insertPlaylistTag :: (MonadDB m) => Id Playlist -> PlaylistTagDTO -> m ()
insertPlaylistTag playlistId tag = withConn $ \conn -> do
  tagIdVal <- runQuery conn $ insert "playlistTag" (PlaylistTagDTO.toBson tag)
  let tagId = fromJust $ cast' tagIdVal :: Id PlaylistTag
  runQuery
    conn
    (insert_
      "playlist_playlistTag"
      ["playlistId" =: playlistId, "tagId" =: tagId]
    )
