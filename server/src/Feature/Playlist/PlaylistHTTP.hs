module Feature.Playlist.PlaylistHTTP
  ( routes
  )
where

import Protolude
import Infrastructure.AppError
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Web.Scotty.Trans (post, ScottyT, ActionT)
import qualified Web.Scotty.Trans as ScottyT

mkCreatePlaylistHttpResult
  :: Monad m => Either GeneralAppError () -> ActionT LText m ()
mkCreatePlaylistHttpResult (Left err) = ScottyT.json $ ErrorResponse (Just err)
mkCreatePlaylistHttpResult _          = ScottyT.json emptyErrorResponse

routes :: (PlaylistService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/playlist" $ do
    body   <- ScottyT.body
    result <- lift $ createPlaylist body

    mkCreatePlaylistHttpResult result
