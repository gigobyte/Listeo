module Feature.User.Types
    ( User(..)
    , fromBson
    , toBson
    ) where

import           Data.Bson       (Document, lookup, (=:))
import qualified Data.Time.Clock as Time
import           Protolude

data User = User
    { id        :: Int
    , username  :: Text
    , password  :: Text
    , createdOn :: Time.UTCTime
    }

fromBson :: Document -> Maybe User
fromBson doc = User
    <$> lookup "id" doc
    <*> lookup "username" doc
    <*> lookup "password" doc
    <*> lookup "createdOn" doc

toBson :: User -> Document
toBson user =
    [ "username" =: username user
    , "password" =: password user
    , "createdOn" =: createdOn user
    ]
