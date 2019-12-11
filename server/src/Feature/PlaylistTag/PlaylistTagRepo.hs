module Feature.PlaylistTag.PlaylistTagRepo where

import Protolude hiding (find)
import Feature.Playlist.Playlist (Playlist)
import Feature.PlaylistTag.PlaylistTag (PlaylistTag)
import Feature.PlaylistTag.PlaylistTagRepoClass (InsertPlaylistTag(..))
import Infrastructure.DB (MonadDB, withConn)
import Infrastructure.Utils.Id (Id)
import Database.PostgreSQL.Simple

insertPlaylistTag :: (MonadDB m) => Id Playlist -> InsertPlaylistTag -> m ()
insertPlaylistTag playlistId tag = withConn $ \conn -> do
  let tagQry = "insert into playlist_tags values (?)"
  let relQry = "insert into playlists_playlist_tags values (?, ?)"

  tagId <- execute conn tagQry (Only $ insertPlaylistTagName tag)
  void $ execute conn relQry (playlistId, tagId)

findPlaylistTagsByPlaylist :: (MonadDB m) => Text -> m [PlaylistTag]
findPlaylistTagsByPlaylist playlistId = withConn $ \conn -> do
  let qry = "select * from playlist_tags\
            \join playlists_playlist_tags\
            \on playlists_playlist_tags.playlist_tag_id = playlist_tags.tag_id\
            \join playlists\
            \on playlists_playlist_tags.playlist_id = playlists.id\
            \where playlists.id = ?"

  liftIO $ query conn qry (Only playlistId)
