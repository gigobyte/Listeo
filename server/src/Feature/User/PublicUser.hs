module Feature.User.PublicUser where

import Protolude
import Data.Time.Clock (UTCTime)
import Data.Aeson (ToJSON)

instance ToJSON PublicUser
data PublicUser = PublicUser
  { username  :: Text
  , createdOn :: UTCTime
  } deriving Generic
