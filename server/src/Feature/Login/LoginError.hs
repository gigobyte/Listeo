module Feature.Login.LoginError where

import Protolude
import Data.Aeson
  (ToJSON, toJSON, genericToJSON, defaultOptions, tagSingleConstructors)

instance ToJSON LoginError where
    toJSON = genericToJSON (defaultOptions { tagSingleConstructors = True })
data LoginError
    = UserNotFound
    deriving Generic
