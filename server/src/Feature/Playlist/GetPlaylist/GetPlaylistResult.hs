module Feature.Playlist.GetPlaylist.GetPlaylistResult where

import Protolude
import Data.Aeson (ToJSON)
import Infrastructure.Utils.Id
import Data.Time.Clock (UTCTime)
import Infrastructure.AppError
import Feature.Playlist.Playlist
import Feature.PlaylistTag.PlaylistTag
import Feature.Video.Video
import Network.HTTP.Types.Status (badRequest400, status500, status401)
import Web.Scotty.Trans

instance ToJSON GetPlaylistError
data GetPlaylistError
  = PlaylistNotFound
  | PlaylistIsPrivate
  | InvalidRequest
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
      , videos :: [Maybe PublicVideo]
      }
  deriving Generic

toHttpResult
  :: Monad m
  => Either GetPlaylistError GetPlaylistResponse
  -> ActionT LText m ()
toHttpResult (Left PlaylistNotFound) = do
  status badRequest400
  json $ ErrorResponse PlaylistNotFound
toHttpResult (Left PlaylistIsPrivate) = do
  status status401
  json $ ErrorResponse PlaylistIsPrivate
toHttpResult (Left InvalidRequest) = do
  status status500
  finish

toHttpResult (Right res) = json res
