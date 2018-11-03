module Feature.User.DB
    ( doesUserExist
    , findUser
    , insertUser
    ) where

import           Database.MongoDB   (Action, Value, count, findOne, insert,
                                     select, (=:))
import qualified Feature.User.Types as User
import           Protolude

insertUser :: User.User -> Action IO Value
insertUser =
    insert "user" . User.toBson

doesUserExist :: Text -> Action IO Bool
doesUserExist username =
    (== 1) <$> count (select ["username" =: username] "user")

findUser :: Text -> Text -> Action IO (Maybe User.User)
findUser username password = do
    doc <- findOne (select ["username" =: username, "password" =: password] "user")
    pure $ doc >>= User.fromBson
