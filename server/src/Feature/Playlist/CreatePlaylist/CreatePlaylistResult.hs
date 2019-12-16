module Feature.Playlist.CreatePlaylist.CreatePlaylistResult where

import Protolude
import Data.Aeson (ToJSON)
import Infrastructure.Utils.Id (Id)
import Infrastructure.AppError
import Network.HTTP.Types.Status (badRequest400)
import Feature.Playlist.Playlist (Playlist)
import Web.Scotty.Trans

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
  status badRequest400
  json $ ErrorResponse err
toHttpResult (Right id) = json $ CreatePlaylistResponse (show id)
