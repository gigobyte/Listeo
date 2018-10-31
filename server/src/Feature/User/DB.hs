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
        [ "username" =: (User.unUsername $ User.username user)
        , "password" =: (User.unPassword $ User.password user)
        , "createdOn" =: User.createdOn user
        ]

doesUserExist :: User.Username -> Action IO Bool
doesUserExist username =
    (== 1) <$> count (select ["username" =: User.unUsername username] "user")
