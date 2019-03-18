module Feature.User.UserDTO where

import Protolude
import Database.MongoDB (Document, (=:))

data UserDTO = UserDTO
    { username :: Text
    , password :: Text
    }

toBson :: UserDTO -> Document
toBson user = ["username" =: username user, "password" =: password user]
