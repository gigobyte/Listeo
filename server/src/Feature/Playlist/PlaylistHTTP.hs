module Feature.Playlist.PlaylistHTTP
  ( routes
  )
where

import Protolude
import Infrastructure.Utils.Id (Id(..))
import Infrastructure.AppError
import Network.HTTP.Types.Status (badRequest400)
import Feature.Playlist.Playlist (Playlist)
import Feature.Playlist.CreatePlaylist.CreatePlaylistResponse
import Feature.Playlist.CreatePlaylist.CreatePlaylistError (CreatePlaylistError)
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Web.Scotty.Trans (post, ScottyT, ActionT)
import qualified Web.Scotty.Trans as ScottyT

mkCreatePlaylistHttpResult
  :: Monad m => Either CreatePlaylistError (Id Playlist) -> ActionT LText m ()
mkCreatePlaylistHttpResult (Left err) = do
  ScottyT.status badRequest400
  ScottyT.json $ ErrorResponse err
mkCreatePlaylistHttpResult (Right id) =
  ScottyT.json $ CreatePlaylistResponse (show $ unId id)

routes :: (PlaylistService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/playlist" $ do
    body   <- ScottyT.body
    result <- lift $ createPlaylist body

    mkCreatePlaylistHttpResult result
