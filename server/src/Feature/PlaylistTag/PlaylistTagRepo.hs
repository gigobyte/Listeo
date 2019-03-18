module Feature.PlaylistTag.PlaylistTagRepo where

import Protolude
import Data.Maybe (fromJust)
import Feature.Playlist.Playlist (Playlist)
import Feature.PlaylistTag.PlaylistTag (PlaylistTag)
import Feature.PlaylistTag.PlaylistTagDTO (PlaylistTagDTO(..))
import qualified Feature.PlaylistTag.PlaylistTagDTO as PlaylistTagDTO
import Infrastructure.DB (MonadDB, runQuery, withConn)
import Infrastructure.Utils.Id (Id)
import Infrastructure.MonadTime
import Database.MongoDB (Document, insert, insert_, cast', (=:))

playlistTagRelation :: Id Playlist -> Id PlaylistTag -> Document
playlistTagRelation playlistId tagId =
  ["playlistId" =: playlistId, "tagId" =: tagId]

insertPlaylistTag
  :: (MonadDB m, MonadTime m) => Id Playlist -> PlaylistTagDTO -> m ()
insertPlaylistTag playlistId tag = do
  dateNow <- getCurrentTime
  withConn $ \conn -> do
    tagIdVal <- runQuery conn (insert "tag" (PlaylistTagDTO.toBson tag dateNow))
    let tagId = fromJust $ cast' tagIdVal
    runQuery
      conn
      (insert_ "playlist-tag" $ playlistTagRelation playlistId tagId)
