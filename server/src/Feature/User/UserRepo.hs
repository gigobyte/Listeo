module Feature.User.UserRepo
  ( findUser
  , insertUser
  )
where

import Protolude
import Feature.User.User (User)
import Database.PostgreSQL.Simple
import Feature.User.UserRepoClass (InsertUser(..))
import Infrastructure.DB (MonadDB, withConn)

insertUser :: (MonadDB m) => InsertUser -> m ()
insertUser (InsertUser insertUserUsername insertUserPassword) = withConn
  $ \conn -> void $ execute conn qry (insertUserUsername, insertUserPassword)
  where qry = "insert into users (username, pass) values (?, ?)"

findUser :: MonadDB m => Text -> m (Maybe User)
findUser username = withConn $ \conn -> do
  let qry = "select * from users where username = ? limit 1"

  results <- liftIO $ query conn qry (Only username)

  return $ head results
