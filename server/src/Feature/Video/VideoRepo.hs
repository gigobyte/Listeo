module Feature.Video.VideoRepo where

import Protolude
import Data.Aeson
import Feature.Playlist.Playlist
import Feature.Video.VideoRepoClass
import Infrastructure.DB
import Infrastructure.Utils.Id
import Feature.Video.Video
  (getVideoSource, Video, VideoMetadata(..), VideoSource(..))
import Database.PostgreSQL.Simple
import Network.HTTP.Simple
import qualified Data.Text as T

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

instance FromJSON YoutubeResponse
data YoutubeResponse = YoutubeResponse
  { items :: [YoutubeResponseItem]
  } deriving Generic

instance FromJSON YoutubeResponseItem
data YoutubeResponseItem = YoutubeResponseItem
  {  snippet :: YoutubeResponseSnippet
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

getVideoMetadata :: (MonadIO m) => Video -> m (Maybe VideoMetadata)
getVideoMetadata video = case (getVideoSource video) of
  Just (YouTube vidId) -> do
    let
      apiUrl =
        "https://www.googleapis.com/youtube/v3/videos?id="
          <> vidId
          <> "&key=AIzaSyCkvj6w56Wc4l663k9jy5ehkFSlGY-qkk4&part=snippet"

    rawRes <- httpLBS (parseRequest_ $ T.unpack $ apiUrl)

    let
      resSnippet :: Maybe YoutubeResponseSnippet =
        snippet <$> (head =<< items <$> (decode $ getResponseBody rawRes))

    return
      $   VideoMetadata
      <$> (title <$> resSnippet)
      <*> (url <$> maxres <$> thumbnails <$> resSnippet)
      <*> (pure $ YouTube vidId)

  Just (Vimeo _) -> undefined
  Nothing        -> undefined
