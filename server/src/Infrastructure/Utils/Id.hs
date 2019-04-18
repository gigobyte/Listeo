module Infrastructure.Utils.Id
  ( Id(..)
  )
where

import Protolude
import Database.MongoDB (ObjectId, Val)
import Data.Aeson (ToJSON(..))

instance ToJSON (Id a) where
  toJSON (Id val) = toJSON (show val :: Text)
newtype Id a = Id
    { unId :: ObjectId
    } deriving (Generic, Typeable, Show, Eq, Val)

