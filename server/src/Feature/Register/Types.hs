module Feature.Register.Types where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
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
    | ServerError
    deriving Generic

instance ToJSON RegisterResponse
data RegisterResponse = RegisterResponse
    { errorDescription :: Maybe RegisterError
    } deriving Generic
