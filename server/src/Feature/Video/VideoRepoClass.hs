module Feature.Video.VideoRepoClass where

import Protolude
import Feature.Playlist.Playlist
import Feature.Video.Video
import Infrastructure.Utils.Id

data InsertVideo = InsertVideo
  { insertVideoUrl :: Text
  , insertVideoNote :: Text
  , insertVideoPlaylistId :: Id Playlist
  }

data InsertVideoTag = InsertVideoTag
  { insertVideoTagName :: Text
  , insertVideoTagVideoId :: Id Video
  }

class Monad m => VideoRepo m where
  findVideosByPlaylist :: Id Playlist -> m [Video]
  getVideoMetadata :: Video -> m (Maybe VideoMetadata)
  insertVideo :: InsertVideo -> m (Id Video)
  insertVideoTag :: InsertVideoTag -> m ()
