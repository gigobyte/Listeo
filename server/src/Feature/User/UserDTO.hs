module Feature.User.UserDTO where

import Protolude
import Data.Time.Clock (UTCTime)
import Database.MongoDB (Document, (=:))

data UserDTO = UserDTO
    { username :: Text
    , password :: Text
    }

toBson :: UserDTO -> UTCTime -> Document
toBson user dateNow =
  [ "username" =: username user
  , "password" =: password user
  , "createdOn" =: dateNow
  ]
