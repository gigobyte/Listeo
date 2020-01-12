module Feature.Video.VideoRepo where

import Protolude
import Feature.Playlist.Playlist (Playlist)
import Infrastructure.DB (MonadDB, withConn)
import Infrastructure.Utils.Id (Id)
import Feature.Video.Video (Video)
import Database.PostgreSQL.Simple

findVideosByPlaylist :: (MonadDB m) => Id Playlist -> m [Video]
findVideosByPlaylist playlistId = withConn $ \conn -> do
  let qry = "SELECT * FROM videos WHERE playlist_id = ? LIMIT 1"

  query conn qry (Only playlistId)
