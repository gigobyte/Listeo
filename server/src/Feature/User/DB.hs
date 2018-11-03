module Feature.User.DB
    ( insertUser
    , doesUserExist
    , findUser
    ) where

import           Database.MongoDB   (Action, Document, Value, count, findOne,
                                     insert, select, (=:))
import qualified Feature.User.Types as User
import           Protolude

insertUser :: User.User -> Action IO Value
insertUser user =
    insert "user"
        [ "username" =: (User.username user)
        , "password" =: (User.password user)
        , "createdOn" =: User.createdOn user
        ]

doesUserExist :: Text -> Action IO Bool
doesUserExist username =
    (== 1) <$> count (select ["username" =: username] "user")

findUser :: Text -> Text -> Action IO (Maybe Document)
findUser username password =
    findOne (select ["username" =: username, "password" =: password] "user")

