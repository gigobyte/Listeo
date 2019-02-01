module Feature.Login.LoginBody where

import Protolude
import Data.Aeson (FromJSON)

instance FromJSON LoginBody
data LoginBody = LoginBody
    { username :: Text
    , password :: Text
    } deriving Generic
