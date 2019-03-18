module Feature.PlaylistTag.PlaylistTagDTO where

import Protolude
import Database.MongoDB (Document, (=:))

data PlaylistTagDTO = PlaylistTagDTO
    { name :: Text
    } deriving Generic

toBson :: PlaylistTagDTO -> Document
toBson tag = ["name" =: name tag]
