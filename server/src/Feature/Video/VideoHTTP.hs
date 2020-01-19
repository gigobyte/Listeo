module Feature.Video.VideoHTTP where

import Protolude hiding (get)
import Web.Scotty.Trans
import Feature.Video.VideoServiceClass
import Feature.Auth.AuthServiceClass
import qualified Feature.Video.AddVideo.AddVideoResult as AddVideoResult

routes :: (VideoService m, AuthService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/playlist/:playlistId/video" $ do
    user       <- requireUser
    rawBody    <- body
    playlistId <- param "playlistId"
    result     <- lift $ addVideoToPlaylist playlistId rawBody user
    AddVideoResult.toHttpResult result
