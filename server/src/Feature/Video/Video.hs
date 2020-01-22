module Feature.Video.Video where

import Protolude
import Infrastructure.Utils.Id
import Feature.Playlist.Playlist
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple

instance ToJSON PublicVideo
data PublicVideo = PublicVideo
  { id :: Id Video
  , url :: Text
  , note :: Text
  , createdOn :: UTCTime
  } deriving Generic

instance FromRow Video
data Video = Video
  { videoId :: Id Video
  , videoUrl :: Text
  , videoPlaylistId :: Id Playlist
  , videoNote :: Text
  , videoCreatedOn :: UTCTime
  } deriving Generic

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

toPublicVideo :: Video -> PublicVideo
toPublicVideo dbVideo = PublicVideo
  { id        = videoId dbVideo
  , url       = videoUrl dbVideo
  , note      = videoNote dbVideo
  , createdOn = videoCreatedOn dbVideo
  }

toPublicVideoTag :: VideoTag -> PublicVideoTag
toPublicVideoTag dbTag = PublicVideoTag { name = videoTagName dbTag }
