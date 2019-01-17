module Infrastructure.AppError where

import Protolude
import Data.Aeson

data AppError a
    = InvalidRequest
    | ValidationFailed
    | ServerError
    | DomainError a
    deriving Generic

instance ToJSON a => ToJSON (AppError a) where
    toJSON (DomainError err) = toJSON err
    toJSON err = genericToJSON defaultOptions err
