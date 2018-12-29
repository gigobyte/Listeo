module Feature.Playlist.Playlist
  ( Playlist(..)
  )
where

import Protolude
import Data.Bson (Document, ObjectId, lookup, (=:))
import qualified Data.Time.Clock as Time

data PlaylistPrivacy
    = Public
    | Private deriving (Eq, Enum)

data PlaylistStyle
    = Unordered
    | Ranked deriving (Eq, Enum)

data Playlist = Playlist
  { id :: ObjectId
  , name :: Text
  , description :: Text
  , privacy :: PlaylistPrivacy
  , style :: PlaylistStyle
  , createdOn :: Time.UTCTime
  }

fromBson :: Document -> Maybe Playlist
fromBson doc =
  Playlist
    <$> lookup "_id"         doc
    <*> lookup "name"        doc
    <*> lookup "description" doc
    <*> lookup "privacy"     doc
    <*> lookup "style"       doc
    <*> lookup "createdOn"   doc

toBson :: Playlist -> Document
toBson playlist =
  [ "name" =: name playlist
  , "description" =: description playlist
  , "privacy" =: privacy playlist
  , "style" =: style playlist
  , "createdOn" =: createdOn playlist
  ]
