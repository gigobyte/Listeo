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
  let tagQry = "INSERT INTO playlist_tags (t_name) VALUES (?)"
  let
    relQry
      = "INSERT INTO playlists_playlist_tags (playlist_id, playlist_tag_id) VALUES (?, ?)"

  tagId <- execute conn tagQry (Only $ insertPlaylistTagName tag)
  void $ execute conn relQry (playlistId, tagId)

findPlaylistTagsByPlaylist :: (MonadDB m) => Text -> m [PlaylistTag]
findPlaylistTagsByPlaylist playlistId = withConn $ \conn -> do
  let
    qry
      = "SELECT * FROM playlist_tags\
        \JOIN playlists_playlist_tags\
        \ON playlists_playlist_tags.playlist_tag_id = playlist_tags.id\
        \JOIN playlists\
        \ON playlists_playlist_tags.playlist_id = playlists.id\
        \WHERE playlists.id = ?"

  liftIO $ query conn qry (Only playlistId)
