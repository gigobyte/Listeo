module Feature.User.User where

import Protolude
import Infrastructure.Utils.Id (Id)
import Database.MongoDB (Document, lookup)
import Data.Time.Clock (UTCTime)

data User = User
  { id        :: Id User
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
