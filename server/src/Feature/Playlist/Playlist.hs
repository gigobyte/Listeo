module Feature.Playlist.Playlist where

import Protolude
import Data.ByteString.Builder (byteString)
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
  fromField _ mdata =
    return $ case mdata of
      Just "public" -> Public
      Just "private" -> Private
      _ -> undefined
 
instance ToField PlaylistPrivacy where
  toField Public = Plain (byteString "public")
  toField Private = Plain (byteString "private")

instance ToJSON PlaylistStyle
instance FromJSON PlaylistStyle
data PlaylistStyle
    = Unordered
    | Ranked
    deriving (Generic, Enum)

instance FromField PlaylistStyle where
  fromField _ mdata =
    return $ case mdata of
      Just "unordered" -> Unordered
      Just "ranked" -> Ranked
      _ -> undefined

instance ToField PlaylistStyle where
  toField Unordered = Plain (byteString "unordered")
  toField Ranked = Plain (byteString "ranked")
    
data Playlist = Playlist
  { id :: Id Playlist
  , name :: Text
  , style :: PlaylistStyle
  , privacy :: PlaylistPrivacy
  , createdOn :: UTCTime
  }

instance FromRow Playlist where
  fromRow = Playlist <$> field <*> field <*> field <*> field <*> field