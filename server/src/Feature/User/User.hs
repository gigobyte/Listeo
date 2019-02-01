module Feature.User.User where

import Protolude
import Database.MongoDB (ObjectId, Document, lookup)
import Data.Time.Clock (UTCTime)

data User = User
  { id        :: ObjectId
  , username  :: Text
  , password  :: Text
  , createdOn :: UTCTime
  }

fromBson :: Document -> Maybe User
fromBson doc =
  User
    <$> lookup "_id"       doc
    <*> lookup "username"  doc
    <*> lookup "password"  doc
    <*> lookup "createdOn" doc
