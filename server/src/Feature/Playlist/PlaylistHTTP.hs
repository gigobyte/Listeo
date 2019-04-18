module Feature.Playlist.PlaylistHTTP
  ( routes
  )
where

import Protolude hiding (get)
import Infrastructure.Utils.Id (Id(..))
import Infrastructure.AppError
import Network.HTTP.Types.Status (badRequest400)
import Feature.Playlist.Playlist (Playlist)
import Feature.Playlist.CreatePlaylist.CreatePlaylistResponse
import Feature.Playlist.CreatePlaylist.CreatePlaylistError (CreatePlaylistError)
import Feature.Playlist.GetPlaylist.GetPlaylistError (GetPlaylistError)
import Feature.Playlist.GetPlaylist.GetPlaylistResponse (GetPlaylistResponse)
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Web.Scotty.Trans (get, post, param, ScottyT, ActionT)
import qualified Web.Scotty.Trans as ScottyT

mkCreatePlaylistHttpResult
  :: Monad m => Either CreatePlaylistError (Id Playlist) -> ActionT LText m ()
mkCreatePlaylistHttpResult (Left err) = do
  ScottyT.status badRequest400
  ScottyT.json $ ErrorResponse err
mkCreatePlaylistHttpResult (Right id) =
  ScottyT.json $ CreatePlaylistResponse (show $ unId id)

mkGetPlaylistHttpResult
  :: Monad m
  => Either GetPlaylistError GetPlaylistResponse
  -> ActionT LText m ()
mkGetPlaylistHttpResult (Left err) = do
  ScottyT.status badRequest400
  ScottyT.json $ ErrorResponse err
mkGetPlaylistHttpResult (Right res) = ScottyT.json res

routes :: (PlaylistService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/playlist" $ do
    body   <- ScottyT.body
    result <- lift $ createPlaylist body

    mkCreatePlaylistHttpResult result

  get "/playlist/:playlistId" $ do
    playlistId <- param "playlistId"
    result     <- lift $ getPlaylist playlistId

    mkGetPlaylistHttpResult result
