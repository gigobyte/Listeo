module Feature.PlaylistTag.PlaylistTagDTO where

import Protolude
import Data.Time.Clock (UTCTime)
import Database.MongoDB (Document, (=:))

data PlaylistTagDTO = PlaylistTagDTO
    { name :: Text
    } deriving Generic

toBson :: PlaylistTagDTO -> UTCTime -> Document
toBson tag dateNow = ["name" =: name tag, "createdOn" =: dateNow]
