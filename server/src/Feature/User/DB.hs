module Feature.User.DB
  ( findUser
  , insertUser
  , doesUserAlreadyExist
  , User(..)
  )
where

import Protolude
import Database.MongoDB
  (Document, ObjectId, Pipe, findOne, insert, lookup, select, (=:))
import Infrastructure.DB (runQuery)
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

insertUser :: Pipe -> User -> IO ()
insertUser pipe user = runQuery pipe (void $ insert "user" (toBson user))

findUser :: Pipe -> Text -> IO (Maybe User)
findUser pipe username = do
  maybeUser <- runQuery pipe $ findOne (select ["username" =: username] "user")
  return $ fromBson =<< maybeUser

doesUserAlreadyExist :: Pipe -> User -> IO Bool
doesUserAlreadyExist pipe user = do
  userInDB <- findUser pipe (username user)
  return $ isJust userInDB
