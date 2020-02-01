module Feature.Video.DeleteVideo.DeleteVideoResult where

import Protolude
import Data.Aeson (ToJSON)
import Web.Scotty.Trans
import Infrastructure.AppError
import Network.HTTP.Types.Status (badRequest400, status401, status500, ok200)

instance ToJSON DeleteVideoError
data DeleteVideoError
  = VideoNotFound
  | InvalidRequest
  | UnauthorizedToDeleteVideo
  deriving Generic

toHttpResult :: Monad m => Either DeleteVideoError () -> ActionT LText m ()
toHttpResult (Left UnauthorizedToDeleteVideo) = do
  status status401
  finish
toHttpResult (Left VideoNotFound) = do
  status badRequest400
  json $ ErrorResponse VideoNotFound
toHttpResult (Left InvalidRequest) = do
  status status500
  finish
toHttpResult (Right _) = do
  status ok200
  finish
