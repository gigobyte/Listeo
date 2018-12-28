module Feature.User.Types
  ( User(..)
  , PublicUser(..)
  , fromBson
  , toBson
  )
where

import           Data.Aeson                     ( ToJSON )
import           Data.Bson                      ( Document
                                                , ObjectId
                                                , lookup
                                                , (=:)
                                                )
import qualified Data.Time.Clock               as Time
import           Protolude

data User = User
    { id        :: ObjectId
    , username  :: Text
    , password  :: Text
    , createdOn :: Time.UTCTime
    } deriving Eq

instance ToJSON PublicUser
data PublicUser = PublicUser
    { username  :: Text
    , createdOn :: Time.UTCTime
    } deriving Generic

fromBson :: Document -> Maybe User
fromBson doc =
  User
    <$> lookup "_id"       doc
    <*> lookup "username"  doc
    <*> lookup "password"  doc
    <*> lookup "createdOn" doc

toBson :: User -> Document
toBson user =
  [ "username" =: username (user :: User) -- TODO some language extension to fix this
  , "password" =: password user
  , "createdOn" =: createdOn (user :: User)
  ]
