module Feature.User.Types where

import           Data.Aeson (FromJSON, ToJSON)
import           Protolude

instance FromJSON RegisterBody
data RegisterBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic

instance ToJSON RegisterError
data RegisterError
    = ValidationFailed
    | UserAlreadyExists
    | BadRequest
    | ServerError
    deriving Generic

instance ToJSON RegisterResponse
data RegisterResponse = RegisterResponse
    { errorDescription :: RegisterError
    } deriving Generic
