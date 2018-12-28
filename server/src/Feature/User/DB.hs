module Feature.User.DB
  ( findUser
  , insertUser
  )
where

import Protolude
import Database.MongoDB (Action, findOne, insert, select, (=:))
import Feature.User.Models.User (User)
import qualified Feature.User.Models.User as User

insertUser :: User.User -> Action IO ()
insertUser = void . insert "user" . User.toBson

findUser :: Text -> Action IO (Maybe User)
findUser username = do
  doc <- findOne (select ["username" =: username] "user")
  pure (doc >>= User.fromBson)
