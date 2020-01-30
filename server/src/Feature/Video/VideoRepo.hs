module Feature.Video.VideoRepo where

import Protolude
import Data.Aeson
import Feature.Playlist.Playlist
import Feature.Video.VideoRepoClass
import Infrastructure.DB
import Infrastructure.HTTP
import Infrastructure.Utils.Id
import Network.HTTP.Simple
import Feature.Video.Video
  (getVideoSource, Video, VideoMetadata(..), VideoSource(..), VideoTag)
import Database.PostgreSQL.Simple
import qualified Infrastructure.Secrets as Secrets

findVideosByPlaylist :: (MonadDB m) => Id Playlist -> m [Video]
findVideosByPlaylist playlistId = withConn $ \conn -> do
  let qry = "SELECT * FROM videos WHERE playlist_id = ?"

  query conn qry (Only playlistId)

findTagsByVideo :: (MonadDB m) => Id Video -> m [VideoTag]
findTagsByVideo videoId = withConn $ \conn -> do
  let qry = "SELECT * FROM video_tags WHERE video_id = ?"

  query conn qry (Only videoId)

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

instance FromJSON YoutubeResponse
data YoutubeResponse = YoutubeResponse
  { items :: [YoutubeResponseItem]
  } deriving Generic

instance FromJSON YoutubeResponseItem
data YoutubeResponseItem = YoutubeResponseItem
  { snippet :: YoutubeResponseSnippet
  , contentDetails :: YoutubeResponseContentDetails
  } deriving Generic

instance FromJSON YoutubeResponseContentDetails
data YoutubeResponseContentDetails = YoutubeResponseContentDetails
  { duration :: Text
  } deriving Generic

instance FromJSON YoutubeResponseSnippet
data YoutubeResponseSnippet = YoutubeResponseSnippet
  { title :: Text
  , thumbnails :: YoutubeResponseThumbnails
  } deriving Generic

instance FromJSON YoutubeResponseThumbnails
data YoutubeResponseThumbnails = YoutubeResponseThumbnails
  { medium :: YoutubeResponseThumbnail
  , high :: YoutubeResponseThumbnail
  , standard :: YoutubeResponseThumbnail
  , maxres :: YoutubeResponseThumbnail
  } deriving Generic

instance FromJSON YoutubeResponseThumbnail
data YoutubeResponseThumbnail = YoutubeResponseThumbnail
  { url :: Text
  , width :: Int
  , height :: Int
  } deriving Generic

getVideoMetadata :: (MonadHTTP m) => Video -> m (Maybe VideoMetadata)
getVideoMetadata video = case (getVideoSource video) of
  Just source@(YouTube vidId) -> do
    rawRes <- httpGet
      (  "https://www.googleapis.com/youtube/v3/videos?id="
      <> vidId
      <> "&key="
      <> Secrets.youtubeAPIKey
      <> "&part=snippet,contentDetails"
      )

    let item = head =<< items <$> (decode $ getResponseBody rawRes)

    return
      $   VideoMetadata
      <$> (title <$> snippet <$> item)
      <*> (url <$> maxres <$> thumbnails <$> snippet <$> item)
      <*> (pure source)
      <*> (duration <$> contentDetails <$> item)

  Just (Vimeo _) -> pure Nothing
  Nothing        -> pure Nothing
