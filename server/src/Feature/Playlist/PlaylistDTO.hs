module Feature.Playlist.PlaylistDTO where

import Protolude
import Feature.Playlist.Playlist (PlaylistStyle, PlaylistPrivacy)
import Database.MongoDB (Document, (=:))

data PlaylistDTO = PlaylistDTO
    { name :: Text
    , style :: PlaylistStyle
    , privacy :: PlaylistPrivacy
    }

toBson :: PlaylistDTO -> Document
toBson playlist =
  [ "name" =: name playlist
  , "style" =: (fromEnum $ style playlist)
  , "privacy" =: (fromEnum $ privacy playlist)
  ]
