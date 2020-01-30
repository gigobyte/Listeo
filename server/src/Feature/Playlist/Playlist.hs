module Feature.Playlist.Playlist where

import Protolude
import Infrastructure.Utils.Id
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)
import Feature.User.User

instance ToJSON PlaylistPrivacy
instance FromJSON PlaylistPrivacy
data PlaylistPrivacy
  = Public
  | Private
  deriving (Generic, Enum, Eq)

instance FromField PlaylistPrivacy where
  fromField _ mdata = return $ case mdata of
    Just "public"  -> Public
    Just "private" -> Private
    _              -> Private

instance ToField PlaylistPrivacy where
  toField Public  = toField ("public" :: Text)
  toField Private = toField ("private" :: Text)

instance ToJSON PlaylistStyle
instance FromJSON PlaylistStyle
data PlaylistStyle
  = Unordered
  | Ranked
  deriving (Generic, Enum, Eq)

instance FromField PlaylistStyle where
  fromField _ mdata = return $ case mdata of
    Just "unordered" -> Unordered
    Just "ranked"    -> Ranked
    _                -> Unordered

instance ToField PlaylistStyle where
  toField Unordered = toField ("unordered" :: Text)
  toField Ranked    = toField ("ranked" :: Text)

instance FromRow Playlist
data Playlist = Playlist
  { playlistId :: Id Playlist
  , playlistAuthorId :: Id User
  , playlistName :: Text
  , playlistDescription :: Text
  , playlistStyle :: PlaylistStyle
  , playlistPrivacy :: PlaylistPrivacy
  , playlistCreatedOn :: UTCTime
  } deriving Generic

isPlaylistViewable :: Maybe User -> Playlist -> Bool
isPlaylistViewable Nothing playlist = playlistPrivacy playlist == Public
isPlaylistViewable (Just user) playlist =
  isPlaylistPublic || (isPlaylistPrivate && isUserAuthorOfPlaylist)
 where
  isUserAuthorOfPlaylist = playlistAuthorId playlist == userId user
  isPlaylistPublic       = playlistPrivacy playlist == Public
  isPlaylistPrivate      = playlistPrivacy playlist == Private


