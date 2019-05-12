module Feature.Register.RegisterResponse where

import Protolude
import Data.Aeson (ToJSON)

instance ToJSON RegisterResponse
data RegisterResponse
    = RegisterResponse { jwt :: Text }
    deriving Generic
