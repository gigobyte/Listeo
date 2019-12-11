module Feature.Playlist.PlaylistRepo
  ( insertPlaylist
  , findPlaylist
  )
where

import Protolude
import Infrastructure.DB (MonadDB, withConn)
import Infrastructure.Utils.Id (Id)
import Database.PostgreSQL.Simple
import Feature.Playlist.Playlist (Playlist)
import Feature.Playlist.PlaylistRepoClass (InsertPlaylist(..))

insertPlaylist :: (MonadDB m) => InsertPlaylist -> m (Id Playlist)
insertPlaylist playlist = withConn $ \conn -> do
  let
    qry
      = "INSERT INTO playlists (p_name, p_description, style, privacy) VALUES (?, ?, ?, ?)"
  playlistId <- liftIO $ execute
    conn
    qry
    ( insertPlaylistName playlist
    , insertPlaylistDescription playlist
    , insertPlaylistStyle playlist
    , insertPlaylistPrivacy playlist
    )
  return playlistId

findPlaylist :: (MonadDB m) => Text -> m (Maybe Playlist)
findPlaylist playlistId = withConn $ \conn -> do
  let qry = "SELECT * FROM playlists WHERE playlist_id = ? LIMIT 1"
  result <- liftIO $ query conn qry (Only playlistId)
  return $ head result
