module Infrastructure.AppError where

import Protolude
import Data.Aeson

instance ToJSON a => ToJSON (ErrorResponse a)
newtype ErrorResponse a = ErrorResponse
    { errorDescription :: a
    } deriving Generic
