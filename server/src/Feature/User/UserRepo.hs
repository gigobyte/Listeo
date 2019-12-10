module Feature.User.UserRepo
  ( findUser
  , insertUser
  )
where

import Protolude
import Database.MongoDB (Document, findOne, insert_, select, (=:))
import Feature.User.User (User)
import qualified Feature.User.User as User
import Feature.User.UserRepoClass (InsertUser(..))
import Infrastructure.DB (MonadDB, runQuery, withConn)

insertUser :: (MonadDB m) => InsertUser -> m ()
insertUser user = withConn
  $ \conn -> runQuery conn (insert_ "user" (toBson user))
 where
  toBson :: InsertUser -> Document
  toBson u =
    ["username" =: insertUserUsername u, "password" =: insertUserPassword u]

findUser :: MonadDB m => Text -> m (Maybe User)
findUser username = withConn $ \conn -> do
  maybeUser <- runQuery conn $ findOne (select ["username" =: username] "user")
  return $ User.fromBson =<< maybeUser
