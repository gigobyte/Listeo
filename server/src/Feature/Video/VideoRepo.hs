module Feature.Video.VideoRepo where

import Protolude
import Feature.Playlist.Playlist (Playlist)
import Feature.Video.VideoRepoClass
import Infrastructure.DB
import Infrastructure.Utils.Id (Id)
import Feature.Video.Video (Video)
import Database.PostgreSQL.Simple

findVideosByPlaylist :: (MonadDB m) => Id Playlist -> m [Video]
findVideosByPlaylist playlistId = withConn $ \conn -> do
  let qry = "SELECT * FROM videos WHERE playlist_id = ?"

  query conn qry (Only playlistId)

insertVideo :: (MonadDB m) => InsertVideo -> m (Id Video)
insertVideo video = withConn $ \conn -> do
  let
    qry
      = "INSERT INTO videos (url, playlist_id, note)\
            \VALUES (?, ?, ?)\
            \RETURNING id"

  result <- liftIO $ query
    conn
    qry
    (insertVideoUrl video, insertVideoPlaylistId video, insertVideoNote video)

  return $ extractReturning result

insertVideoTag :: (MonadDB m) => InsertVideoTag -> m ()
insertVideoTag tag = withConn $ \conn -> do
  let qry = "INSERT INTO video_tags (name, video_id) VALUES (?, ?)"
  void $ execute conn qry (insertVideoTagName tag, insertVideoTagVideoId tag)
