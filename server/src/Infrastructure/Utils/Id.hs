module Infrastructure.Utils.Id where

import Protolude
import Database.MongoDB (ObjectId, Val)

newtype Id a = Id
    { unId :: ObjectId
    } deriving (Typeable, Show, Eq, Val)
