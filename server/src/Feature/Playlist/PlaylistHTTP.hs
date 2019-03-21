module Feature.Playlist.PlaylistHTTP
  ( routes
  )
where

import Protolude
import Infrastructure.AppError (GeneralAppError)
import Infrastructure.Utils.Id (Id(..))
import Feature.Playlist.Playlist (Playlist)
import Feature.Playlist.CreatePlaylist.CreatePlaylistResponse
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Web.Scotty.Trans (post, ScottyT, ActionT)
import qualified Web.Scotty.Trans as ScottyT

mkCreatePlaylistHttpResult
  :: Monad m => Either GeneralAppError (Id Playlist) -> ActionT LText m ()
mkCreatePlaylistHttpResult (Left err) = ScottyT.json $ ErrorResponse err
mkCreatePlaylistHttpResult (Right id) =
  ScottyT.json $ SuccessResponse (show $ unId id)

routes :: (PlaylistService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/playlist" $ do
    body   <- ScottyT.body
    result <- lift $ createPlaylist body

    mkCreatePlaylistHttpResult result
