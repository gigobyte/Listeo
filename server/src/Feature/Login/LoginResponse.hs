module Feature.Login.LoginResponse where

import Protolude
import Infrastructure.AppError (AppError)
import Feature.Login.LoginError (LoginError)
import Data.Aeson (ToJSON)


instance ToJSON LoginResponse
data LoginResponse
    = ErrorResponse { errorDescription :: AppError LoginError }
    | SuccessResponse { jwt :: Text }
    deriving Generic
