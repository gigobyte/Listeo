module Infrastructure.AppError where

import Protolude
import Data.Aeson

data AppError a
    = InvalidRequest
    | ValidationFailed
    | ServerError
    | DomainError a
    deriving Generic

type GeneralAppError = AppError ()

instance ToJSON a => ToJSON (AppError a) where
    toJSON (DomainError err) = toJSON err
    toJSON err = genericToJSON defaultOptions err

instance ToJSON a => ToJSON (ErrorResponse a)
newtype ErrorResponse a = ErrorResponse
    { errorDescription :: Maybe (AppError a)
    } deriving Generic

emptyErrorResponse :: ErrorResponse ()
emptyErrorResponse = ErrorResponse Nothing
