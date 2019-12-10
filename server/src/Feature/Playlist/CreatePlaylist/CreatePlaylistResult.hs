module Feature.Playlist.CreatePlaylist.CreatePlaylistResult where

import Protolude
import Data.Aeson
import Infrastructure.Utils.Id (Id(..))
import Infrastructure.AppError
import Network.HTTP.Types.Status (badRequest400)
import Feature.Playlist.Playlist (Playlist)
import Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as ScottyT

instance ToJSON CreatePlaylistError
data CreatePlaylistError
    = InvalidRequest
    | ValidationFailed
    deriving Generic

instance ToJSON CreatePlaylistResponse
data CreatePlaylistResponse
    = CreatePlaylistResponse { playlistId :: Text }
    deriving Generic

toHttpResult
  :: Monad m => Either CreatePlaylistError (Id Playlist) -> ActionT LText m ()
toHttpResult (Left err) = do
  ScottyT.status badRequest400
  ScottyT.json $ ErrorResponse err
toHttpResult (Right id) =
  ScottyT.json $ CreatePlaylistResponse (show $ unId id)
