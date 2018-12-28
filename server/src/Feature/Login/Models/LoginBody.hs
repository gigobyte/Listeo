module Feature.Login.Models.LoginBody
  ( LoginBody(..)
  )
where

import Protolude
import qualified Data.Aeson as Aeson

instance Aeson.FromJSON LoginBody
data LoginBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic
