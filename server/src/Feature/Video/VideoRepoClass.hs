module Feature.Video.VideoRepoClass where

import Protolude
import Feature.Playlist.Playlist (Playlist)
import Feature.Video.Video (Video)
import Infrastructure.Utils.Id (Id)

class Monad m => VideoRepo m where
  findVideosByPlaylist :: Id Playlist -> m [Video]
