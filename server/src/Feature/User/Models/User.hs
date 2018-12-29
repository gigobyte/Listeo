module Feature.User.Models.User
  ( User(..)
  , fromBson
  , toBson
  )
where

import Protolude
import Data.Bson (Document, ObjectId, lookup, (=:))
import qualified Data.Time.Clock as Time

data User = User
  { id        :: ObjectId
  , username  :: Text
  , password  :: Text
  , createdOn :: Time.UTCTime
  }


fromBson :: Document -> Maybe User
fromBson doc =
  User
    <$> lookup "_id"       doc
    <*> lookup "username"  doc
    <*> lookup "password"  doc
    <*> lookup "createdOn" doc

toBson :: User -> Document
toBson user =
  [ "username" =: username user
  , "password" =: password user
  , "createdOn" =: createdOn user
  ]

