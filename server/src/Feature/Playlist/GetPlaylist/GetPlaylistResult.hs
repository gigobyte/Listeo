module Feature.Playlist.GetPlaylist.GetPlaylistResult where

import Protolude
import Data.Aeson (ToJSON)
import Infrastructure.Utils.Id (Id)
import Data.Time.Clock (UTCTime)
import Infrastructure.AppError
import Feature.Playlist.Playlist (Playlist, PlaylistStyle, PlaylistPrivacy)
import Feature.PlaylistTag.PlaylistTag (PublicPlaylistTag)
import Network.HTTP.Types.Status (badRequest400)
import Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as ScottyT

instance ToJSON GetPlaylistError
data GetPlaylistError
  = PlaylistNotFound
  deriving Generic

instance ToJSON GetPlaylistResponse
data GetPlaylistResponse
  = GetPlaylistResponse
      { id :: Id Playlist
      , name :: Text
      , style :: PlaylistStyle
      , privacy :: PlaylistPrivacy
      , createdOn :: UTCTime
      , tags :: [PublicPlaylistTag]
      }
  deriving Generic

toHttpResult
  :: Monad m
  => Either GetPlaylistError GetPlaylistResponse
  -> ActionT LText m ()
toHttpResult (Left err) = do
  ScottyT.status badRequest400
  ScottyT.json $ ErrorResponse err
toHttpResult (Right res) = ScottyT.json res
