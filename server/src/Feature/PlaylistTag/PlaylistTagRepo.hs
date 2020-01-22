module Feature.PlaylistTag.PlaylistTagRepo where

import Protolude
import Feature.Playlist.Playlist
import Feature.PlaylistTag.PlaylistTag
import Feature.PlaylistTag.PlaylistTagRepoClass
import Infrastructure.DB
import Infrastructure.Utils.Id
import Database.PostgreSQL.Simple

insertPlaylistTag :: (MonadDB m) => InsertPlaylistTag -> m ()
insertPlaylistTag tag = withConn $ \conn -> do
  let playlistId = insertPlaylistTagPlaylistId tag

  let getQry     = "SELECT * FROM playlist_tags WHERE name = ? LIMIT 1"
  let tagQry     = "INSERT INTO playlist_tags (name)  VALUES (?) RETURNING id"
  let
    relQry
      = "INSERT INTO playlists_playlist_tags (playlist_id, playlist_tag_id) VALUES (?, ?)"

  maybeExistingTag <- head
    <$> query conn getQry (Only $ insertPlaylistTagName tag)

  case maybeExistingTag of
    Just existingTag ->
      void $ execute conn relQry (playlistId, playlistTagId existingTag)

    Nothing -> do
      result <- query conn tagQry (Only $ insertPlaylistTagName tag)
      void $ execute conn relQry (playlistId, extractReturning result)

findPlaylistTagsByPlaylist :: (MonadDB m) => Id Playlist -> m [PlaylistTag]
findPlaylistTagsByPlaylist playlistId = withConn $ \conn -> do
  let
    qry
      = "SELECT playlist_tags.id, playlist_tags.name FROM playlist_tags \
        \JOIN playlists_playlist_tags \
        \ON playlists_playlist_tags.playlist_tag_id = playlist_tags.id \
        \WHERE playlists_playlist_tags.playlist_id = ?"

  query conn qry (Only playlistId)
