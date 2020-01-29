module Feature.Video.Video where

import Protolude
import Infrastructure.Utils.Id
import Feature.Playlist.Playlist
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple
import Network.URI
import Infrastructure.Utils.URI
import qualified Data.Text as T

instance ToJSON VideoSource
data VideoSource
  = YouTube Text
  | Vimeo Text
  deriving Generic

instance ToJSON PublicVideo
data PublicVideo = PublicVideo
  { id :: Id Video
  , url :: Text
  , note :: Text
  , createdOn :: UTCTime
  , thumbnail :: Text
  , title :: Text
  , source :: VideoSource
  } deriving Generic

instance FromRow Video
data Video = Video
  { videoId :: Id Video
  , videoUrl :: Text
  , videoPlaylistId :: Id Playlist
  , videoNote :: Text
  , videoCreatedOn :: UTCTime
  } deriving Generic

data VideoMetadata = VideoMetadata
  { videoMetadataTitle :: Text
  , videoMetadataThumbnail :: Text
  , videoMetadataSource :: VideoSource
  }

instance ToJSON PublicVideoTag
data PublicVideoTag = PublicVideoTag
  { name :: Text
  } deriving Generic

instance FromRow VideoTag
data VideoTag = VideoTag
  { videoTagId :: Id VideoTag
  , videoTagName :: Text
  , videoTagVideoId :: Id Video
  } deriving Generic

toPublicVideoTag :: VideoTag -> PublicVideoTag
toPublicVideoTag dbTag = PublicVideoTag { name = videoTagName dbTag }

getVideoSource :: Video -> Maybe VideoSource
getVideoSource video =
  toSource =<< T.pack <$> uriRegName <$> (uriAuthority parsedURI)
 where
  parsedURI   = fromMaybe nullURI (parseURI $ T.unpack $ videoUrl video)
  queryParams = parseQueryString $ T.pack $ uriQuery parsedURI
  parsedPath  = T.pack $ uriPath parsedURI
  toSource :: Text -> Maybe VideoSource
  toSource regName
    | T.isInfixOf "youtube" regName
    = YouTube <$> snd <$> ((find (\(k, _) -> k == "v")) queryParams)
    | T.isInfixOf "vimeo" regName
    = Vimeo <$> (head $ T.splitOn "/" parsedPath)
    | otherwise
    = Nothing

toPublicVideo :: Video -> VideoMetadata -> PublicVideo
toPublicVideo dbVideo meta = PublicVideo
  { id        = videoId dbVideo
  , url       = videoUrl dbVideo
  , note      = videoNote dbVideo
  , createdOn = videoCreatedOn dbVideo
  , thumbnail = videoMetadataThumbnail meta
  , title     = videoMetadataTitle meta
  , source    = videoMetadataSource meta
  }
