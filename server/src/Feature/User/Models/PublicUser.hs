module Feature.User.Models.PublicUser
  ( PublicUser(..)
  )
where

import Protolude
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time

instance Aeson.ToJSON PublicUser
data PublicUser = PublicUser
    { username  :: Text
    , createdOn :: Time.UTCTime
    } deriving Generic
