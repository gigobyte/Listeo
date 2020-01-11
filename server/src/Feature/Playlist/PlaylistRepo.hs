module Feature.Playlist.PlaylistRepo
  ( insertPlaylist
  , findPlaylist
  )
where

import Protolude
import Infrastructure.DB (MonadDB, withConn, extractReturning)
import Infrastructure.Utils.Id (Id)
import Database.PostgreSQL.Simple
import Feature.Playlist.Playlist (Playlist)
import Feature.Playlist.PlaylistRepoClass (InsertPlaylist(..))

insertPlaylist :: (MonadDB m) => InsertPlaylist -> m (Id Playlist)
insertPlaylist playlist = withConn $ \conn -> do
  let
    qry
      = "INSERT INTO playlists (author_id, name, description, style, privacy) \
        \VALUES (?, ?, ?, ?, ?) \
        \RETURNING id"

  result <- liftIO $ query
    conn
    qry
    ( insertPlaylistAuthorId playlist
    , insertPlaylistName playlist
    , insertPlaylistDescription playlist
    , insertPlaylistStyle playlist
    , insertPlaylistPrivacy playlist
    )
  return $ extractReturning result

findPlaylist :: (MonadDB m) => Id Playlist -> m (Maybe Playlist)
findPlaylist playlistId = withConn $ \conn -> do
  let
    qry
      = "SELECT * FROM playlists \
        \WHERE id = ? \
        \LIMIT 1"

  result <- liftIO $ query conn qry (Only playlistId)
  return $ head result
