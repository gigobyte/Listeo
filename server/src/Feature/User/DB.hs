module Feature.User.DB
  ( findUser
  , insertUser
  , doesUserAlreadyExist
  , User(..)
  )
where

import Protolude
import Database.MongoDB
  (Document, ObjectId, findOne, insert, lookup, select, (=:))
import Infrastructure.DB (MonadDB, runQuery, withConn)
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

insertUser :: MonadDB r m => User -> m ()
insertUser user =
  withConn $ \conn -> runQuery conn (void $ insert "user" (toBson user))

findUser :: MonadDB r m => Text -> m (Maybe User)
findUser username = withConn $ \conn -> do
  maybeUser <- runQuery conn $ findOne (select ["username" =: username] "user")
  return $ fromBson =<< maybeUser

doesUserAlreadyExist :: MonadDB r m => User -> m Bool
doesUserAlreadyExist user = do
  userInDB <- findUser (username user)
  return $ isJust userInDB

