module Feature.Register.RegisterError where

import Protolude
import Data.Aeson
  (ToJSON, toJSON, genericToJSON, defaultOptions, tagSingleConstructors)

instance ToJSON RegisterError where
    toJSON = genericToJSON (defaultOptions { tagSingleConstructors = True })
data RegisterError
    = UserAlreadyExists
    deriving Generic

