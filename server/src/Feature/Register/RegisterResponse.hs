module Feature.Register.RegisterResponse where

import Protolude
import Infrastructure.AppError (AppError)
import Feature.Register.RegisterError (RegisterError)
import Data.Aeson (ToJSON)

instance ToJSON RegisterResponse
data RegisterResponse = RegisterResponse
    { errorDescription :: Maybe (AppError RegisterError)
    } deriving Generic
