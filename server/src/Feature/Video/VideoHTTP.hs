module Feature.Video.VideoHTTP where

import Protolude hiding (get)
import Web.Scotty.Trans
import Feature.Video.VideoServiceClass
import Feature.Auth.AuthServiceClass
import qualified Feature.Video.AddVideo.AddVideoResult as AddVideoResult
import qualified Feature.Video.DeleteVideo.DeleteVideoResult as DeleteVideoResult

routes :: (VideoService m, AuthService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/playlist/:playlistId/video" $ do
    user       <- requireUser
    rawBody    <- body
    playlistId <- param "playlistId"
    result     <- lift $ addVideoToPlaylist playlistId rawBody user
    AddVideoResult.toHttpResult result

  delete "/playlist/:playlistId/video/:videoId" $ do
    user       <- requireUser
    playlistId <- param "playlistId"
    videoId    <- param "videoId"
    result     <- lift $ deletePlaylistVideo playlistId videoId user
    DeleteVideoResult.toHttpResult result
