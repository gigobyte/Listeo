module Feature.Playlist.GetPlaylist.GetPlaylistResult where

import Protolude
import Data.Aeson (ToJSON)
import Infrastructure.Utils.Id (Id)
import Data.Time.Clock (UTCTime)
import Infrastructure.AppError
import Feature.Playlist.Playlist (Playlist, PlaylistStyle, PlaylistPrivacy)
import Feature.PlaylistTag.PlaylistTag (PublicPlaylistTag)
import Feature.Video.Video (PublicVideo)
import Network.HTTP.Types.Status (badRequest400)
import Web.Scotty.Trans

instance ToJSON GetPlaylistError
data GetPlaylistError
  = PlaylistNotFound
  deriving Generic

instance ToJSON GetPlaylistResponse
data GetPlaylistResponse
  = GetPlaylistResponse
      { id :: Id Playlist
      , name :: Text
      , description :: Text
      , style :: PlaylistStyle
      , privacy :: PlaylistPrivacy
      , createdOn :: UTCTime
      , tags :: [PublicPlaylistTag]
      , videos :: [PublicVideo]
      }
  deriving Generic

toHttpResult
  :: Monad m
  => Either GetPlaylistError GetPlaylistResponse
  -> ActionT LText m ()
toHttpResult (Left PlaylistNotFound) = do
  status badRequest400
  json $ ErrorResponse PlaylistNotFound
toHttpResult (Right res) = json res
