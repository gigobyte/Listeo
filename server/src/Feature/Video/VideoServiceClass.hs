module Feature.Video.VideoServiceClass where

import Protolude
import Infrastructure.Utils.Id
import Feature.Video.Video
import Feature.User.User
import Feature.Video.AddVideo.AddVideoResult

class Monad m => VideoService m where
  addVideoToPlaylist :: LByteString -> LByteString -> User -> m (Either AddVideoError (Id Video))
