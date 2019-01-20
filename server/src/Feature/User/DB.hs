module Feature.User.DB
  ( findUser
  , insertUser
  , doesUserAlreadyExist
  , User(..)
  , UserDTO(..)
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

data UserDTO = UserDTO
  { dtoUsername :: Text
  , dtoPassword :: Text }

userFromBson :: Document -> Maybe User
userFromBson doc =
  User
    <$> lookup "_id"       doc
    <*> lookup "username"  doc
    <*> lookup "password"  doc
    <*> lookup "createdOn" doc

userDtoToBson :: UserDTO -> Document
userDtoToBson user =
  ["username" =: dtoUsername user, "password" =: dtoPassword user]

insertUser :: MonadDB m => UserDTO -> m ()
insertUser user =
  withConn $ \conn -> runQuery conn (void $ insert "user" (userDtoToBson user))

findUser :: MonadDB m => Text -> m (Maybe User)
findUser username = withConn $ \conn -> do
  maybeUser <- runQuery conn $ findOne (select ["username" =: username] "user")
  return $ userFromBson =<< maybeUser

doesUserAlreadyExist :: MonadDB m => Text -> m Bool
doesUserAlreadyExist username = do
  userInDB <- findUser username
  return $ isJust userInDB

