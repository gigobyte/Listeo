module Feature.Login.DB where

import           Database.MongoDB (Action, Document, findOne, select, (=:))
import           Protolude

findUser :: Text -> Text -> Action IO (Maybe Document)
findUser username password =
    findOne (select ["username" =: username, "password" =: password] "user")
