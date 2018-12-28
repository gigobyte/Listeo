module Feature.Register.Models.RegisterBody
  ( RegisterBody(..)
  )
where

import Protolude
import qualified Data.Aeson as Aeson

instance Aeson.FromJSON RegisterBody
data RegisterBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic
