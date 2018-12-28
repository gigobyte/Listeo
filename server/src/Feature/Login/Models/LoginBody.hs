module Feature.Login.Models.LoginBody
  ( LoginBody(..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Protolude

instance FromJSON LoginBody
data LoginBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic
