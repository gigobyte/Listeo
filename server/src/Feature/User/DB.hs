module Feature.User.DB
    ( findUser
    , insertUser
    ) where

import           Database.MongoDB   (Action, Value, findOne, insert, select,
                                     (=:))
import qualified Feature.User.Types as User
import           Protolude

insertUser :: User.User -> Action IO Value
insertUser =
    insert "user" . User.toBson

findUser :: Text -> Action IO (Maybe User.User)
findUser username = do
    doc <- findOne (select ["username" =: username] "user")
    pure $ doc >>= User.fromBson
