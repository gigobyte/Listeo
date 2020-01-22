module Feature.Video.AddVideo.AddVideoResult where

import Protolude
import Data.Aeson (ToJSON)
import Web.Scotty.Trans
import Infrastructure.Utils.Id (Id)
import Infrastructure.AppError
import Feature.Video.Video (Video)
import Network.HTTP.Types.Status (badRequest400, status401, status500)

instance ToJSON AddVideoError
data AddVideoError
  = InvalidRequest
  | ValidationFailed
  | UnauthorizedToEditPlaylist
  deriving Generic

instance ToJSON AddVideoResponse
data AddVideoResponse
  = AddVideoResponse { videoId :: Text }
  deriving Generic

toHttpResult :: Monad m => Either AddVideoError (Id Video) -> ActionT LText m ()
toHttpResult (Left UnauthorizedToEditPlaylist) = do
  status status401
  finish
toHttpResult (Left ValidationFailed) = do
  status badRequest400
  json $ ErrorResponse ValidationFailed
toHttpResult (Left InvalidRequest) = do
  status status500
  finish
toHttpResult (Right id) = json $ AddVideoResponse (show id)
