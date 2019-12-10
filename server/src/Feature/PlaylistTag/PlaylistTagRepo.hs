module Feature.PlaylistTag.PlaylistTagRepo where

import Protolude hiding (find)
import Data.Maybe (fromJust)
import Feature.Playlist.Playlist (Playlist)
import Feature.PlaylistTag.PlaylistTag (PlaylistTag)
import qualified Feature.PlaylistTag.PlaylistTag as PlaylistTag
import Feature.PlaylistTag.PlaylistTagRepoClass (InsertPlaylistTag(..))
import Infrastructure.DB (MonadDB, runQuery, withConn)
import Infrastructure.Utils.Id (Id)
import Database.MongoDB (insert, insert_, cast', find, select, rest, (=:))

insertPlaylistTag :: (MonadDB m) => Id Playlist -> InsertPlaylistTag -> m ()
insertPlaylistTag playlistId tag = withConn $ \conn -> do
  tagIdVal <- runQuery conn
    $ insert "playlistTag" ["name" =: insertPlaylistTagName tag]
  let tagId = fromJust $ cast' tagIdVal :: Id PlaylistTag
  runQuery
    conn
    (insert_
      "playlist_playlistTag"
      ["playlistId" =: playlistId, "tagId" =: tagId]
    )

findPlaylistTagsByPlaylist :: (MonadDB m) => Text -> m [PlaylistTag]
findPlaylistTagsByPlaylist playlistId = withConn $ \conn -> do
  tagsCursor <- runQuery conn
    $ find (select ["playlistId" =: playlistId] "playlist_playlistTag")

  tagDocuments <- runQuery conn $ rest tagsCursor

  return $ catMaybes $ PlaylistTag.fromBson <$> tagDocuments
