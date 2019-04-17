module Feature.Login.LoginResponse where

import Protolude
import Data.Aeson (ToJSON)

instance ToJSON LoginResponse
data LoginResponse
    = LoginResponse { jwt :: Text }
    deriving Generic
