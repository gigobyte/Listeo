module Feature.Playlist.PlaylistHTTP
  ( routes
  )
where

import Protolude hiding (get)
import qualified Feature.Playlist.CreatePlaylist.CreatePlaylistResult as CreatePlaylistResult
import qualified Feature.Playlist.GetPlaylist.GetPlaylistResult as GetPlaylistResult
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Web.Scotty.Trans (get, post, param, ScottyT)
import qualified Web.Scotty.Trans as ScottyT

routes :: (PlaylistService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/playlist" $ do
    body   <- ScottyT.body
    result <- lift $ createPlaylist body
    CreatePlaylistResult.toHttpResult result

  get "/playlist/:playlistId" $ do
    playlistId <- param "playlistId"
    result     <- lift $ getPlaylist playlistId
    GetPlaylistResult.toHttpResult result
