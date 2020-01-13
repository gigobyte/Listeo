module Feature.Playlist.PlaylistHTTP
  ( routes
  )
where

import Protolude hiding (get)
import qualified Feature.Playlist.CreatePlaylist.CreatePlaylistResult as CreatePlaylistResult
import qualified Feature.Playlist.GetPlaylist.GetPlaylistResult as GetPlaylistResult
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Feature.Auth.AuthServiceClass (AuthService, requireUser, optionalUser)
import Web.Scotty.Trans

routes :: (PlaylistService m, AuthService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/playlist" $ do
    user    <- requireUser
    rawBody <- body
    result  <- lift $ createPlaylist rawBody user
    CreatePlaylistResult.toHttpResult result

  get "/playlist/:playlistId" $ do
    user       <- optionalUser
    playlistId <- param "playlistId"
    result     <- lift $ getPlaylist playlistId user
    GetPlaylistResult.toHttpResult result
