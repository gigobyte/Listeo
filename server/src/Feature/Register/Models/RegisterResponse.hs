module Feature.Register.Models.RegisterResponse
  ( RegisterResponse(..)
  , RegisterError(..)
  )
where

import Protolude
import qualified Data.Aeson as Aeson

instance Aeson.ToJSON RegisterError
data RegisterError
    = ValidationFailed
    | UserAlreadyExists
    | ServerError
    deriving Generic

instance Aeson.ToJSON RegisterResponse
data RegisterResponse = RegisterResponse
    { errorDescription :: Maybe RegisterError
    } deriving Generic
