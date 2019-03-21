module Feature.Playlist.CreatePlaylist.CreatePlaylistResponse where

import Protolude
import Infrastructure.AppError (GeneralAppError)
import Data.Aeson (ToJSON)

instance ToJSON CreatePlaylistResponse
data CreatePlaylistResponse
    = ErrorResponse { errorDescription :: GeneralAppError }
    | SuccessResponse { playlistId :: Text }
    deriving Generic

