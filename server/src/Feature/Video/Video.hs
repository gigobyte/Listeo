module Feature.Video.Video where

import Protolude
import Infrastructure.Utils.Id (Id)
import Feature.Playlist.Playlist (Playlist)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

instance ToJSON VideoSource
instance FromJSON VideoSource
data VideoSource
    = YouTube
    | Vimeo
    deriving (Generic, Enum)

instance FromField VideoSource where
  fromField _ mdata = return $ case mdata of
    Just "youtube" -> YouTube
    Just "vimeo"   -> Vimeo
    _              -> YouTube

instance ToField VideoSource where
  toField YouTube = toField ("youtube" :: Text)
  toField Vimeo   = toField ("vimeo" :: Text)

instance ToJSON PublicVideo
data PublicVideo = PublicVideo
  { id :: Id Video
  , name :: Text
  , source :: VideoSource
  , link :: Text
  , note :: Text
  , createdOn :: UTCTime
  } deriving Generic

instance FromRow Video
data Video = Video
  { videoId :: Id Video
  , videoName :: Text
  , videoSource :: VideoSource
  , videoLink :: Text
  , videoPlaylistId :: Id Playlist
  , videoNote :: Text
  , videoCreatedOn :: UTCTime
  } deriving Generic

toPublicVideo :: Video -> PublicVideo
toPublicVideo dbVideo = PublicVideo
  { id        = videoId dbVideo
  , name      = videoName dbVideo
  , source    = videoSource dbVideo
  , link      = videoLink dbVideo
  , note      = videoNote dbVideo
  , createdOn = videoCreatedOn dbVideo
  }
