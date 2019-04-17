module Feature.Login.LoginError where

import Protolude
import Data.Aeson (ToJSON)

instance ToJSON LoginError
data LoginError
    = UserNotFound
    | InvalidRequest
    deriving Generic
