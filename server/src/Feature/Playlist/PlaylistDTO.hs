module Feature.Playlist.PlaylistDTO where

import Protolude
import Data.Time.Clock (UTCTime)
import Feature.Playlist.Playlist (PlaylistStyle, PlaylistPrivacy)
import Database.MongoDB (Document, (=:))

data PlaylistDTO = PlaylistDTO
    { name :: Text
    , style :: PlaylistStyle
    , privacy :: PlaylistPrivacy
    }

toBson :: PlaylistDTO -> UTCTime -> Document
toBson playlist dateNow =
  [ "name" =: name playlist
  , "style" =: (fromEnum $ style playlist)
  , "privacy" =: (fromEnum $ privacy playlist)
  , "createdOn" =: dateNow
  ]
