module Feature.Login.Models.LoginResponse
  ( LoginResponse(..)
  , LoginError(..)
  )
where

import Protolude
import qualified Data.Aeson as Aeson

instance Aeson.ToJSON LoginResponse
data LoginResponse
    = ErrorResponse { errorDescription :: LoginError }
    | SuccessResponse { jwt :: Text }
    deriving Generic

instance Aeson.ToJSON LoginError
data LoginError
    = ValidationFailed
    | UserNotFound
    | ServerError
    deriving Generic
