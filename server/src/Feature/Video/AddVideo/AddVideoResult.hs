module Feature.Video.AddVideo.AddVideoResult where

import Protolude
import Data.Aeson (ToJSON)
import Web.Scotty.Trans
import Infrastructure.Utils.Id (Id)
import Feature.Video.Video (Video)
import Network.HTTP.Types.Status (status401, status500)

instance ToJSON AddVideoError
data AddVideoError = InvalidRequest | UnauthorizedToEditPlaylist deriving Generic

instance ToJSON AddVideoResponse
data AddVideoResponse
  = AddVideoResponse { videoId :: Text }
  deriving Generic

toHttpResult :: Monad m => Either AddVideoError (Id Video) -> ActionT LText m ()
toHttpResult (Left UnauthorizedToEditPlaylist) = do
  status status401
  finish
toHttpResult (Left InvalidRequest) = do
  status status500
  finish
toHttpResult (Right id) = json $ AddVideoResponse (show id)
