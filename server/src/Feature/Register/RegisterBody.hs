module Feature.Register.RegisterBody where

import Protolude
import Data.Aeson (FromJSON)

instance FromJSON RegisterBody
data RegisterBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic
