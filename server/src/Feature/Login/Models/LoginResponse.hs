module Feature.Login.Models.LoginResponse
  ( LoginResponse(..)
  , LoginError(..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Protolude

instance ToJSON LoginResponse
data LoginResponse
    = ErrorResponse { errorDescription :: LoginError }
    | SuccessResponse { jwt :: Text }
    deriving Generic

instance ToJSON LoginError
data LoginError
    = ValidationFailed
    | UserNotFound
    | ServerError
    deriving Generic
