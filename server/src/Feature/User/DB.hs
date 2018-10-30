module Feature.User.DB
    ( insertUser
    , doesUserExist
    ) where

import           Database.MongoDB       (Action, Value, count, insert, select,
                                         (=:))
import qualified Feature.User.UserModel as User
import           Protolude

insertUser :: User.User -> Action IO Value
insertUser user =
    insert "user"
        [ "username" =: (User.unwrapUsername $ User.username user)
        , "password" =: (User.unwrapPassword $ User.password user)
        , "createdOn" =: User.createdOn user
        ]

doesUserExist :: Text -> Action IO Bool
doesUserExist username =
    (== 1) <$> count (select ["username" =: username] "user")
