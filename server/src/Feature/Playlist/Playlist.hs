module Feature.Playlist.Playlist where

import Protolude
import Infrastructure.Utils.Id (Id)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)

instance ToJSON PlaylistPrivacy
instance FromJSON PlaylistPrivacy
data PlaylistPrivacy
  = Public
  | Private
  deriving (Generic, Enum)

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
    deriving (Generic, Enum)

instance FromField PlaylistStyle where
  fromField _ mdata = return $ case mdata of
    Just "unordered" -> Unordered
    Just "ranked"    -> Ranked
    _                -> Unordered

instance ToField PlaylistStyle where
  toField Unordered = toField ("unordered" :: Text)
  toField Ranked    = toField ("ranked" :: Text)

data Playlist = Playlist
  { playlistId :: Id Playlist
  , playlistName :: Text
  , playlistStyle :: PlaylistStyle
  , playlistPrivacy :: PlaylistPrivacy
  , playlistCreatedOn :: UTCTime
  }

instance FromRow Playlist where
  fromRow = Playlist <$> field <*> field <*> field <*> field <*> field
