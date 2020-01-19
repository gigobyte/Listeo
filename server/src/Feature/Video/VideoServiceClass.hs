module Feature.Video.VideoServiceClass where

import Protolude
import Infrastructure.Utils.Id (Id)
import Feature.Video.Video (Video)
import Feature.User.User (User)
import Feature.Video.AddVideo.AddVideoResult (AddVideoError)

class Monad m => VideoService m where
  addVideoToPlaylist :: LByteString -> LByteString -> User -> m (Either AddVideoError (Id Video))
