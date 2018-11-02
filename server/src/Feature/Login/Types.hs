module Feature.Login.Types where

import           Data.Aeson (FromJSON, ToJSON)
import           Protolude

instance FromJSON LoginBody
data LoginBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic

instance ToJSON LoginError
data LoginError
    = ValidationFailed
    | UserNotFound
    | ServerError
    deriving Generic

instance ToJSON LoginResponse
data LoginResponse
    = ErrorResponse { errorDescription :: LoginError }
    | SuccessResponse { jwt :: Text }
    deriving Generic

