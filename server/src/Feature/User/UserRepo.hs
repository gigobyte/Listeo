module Feature.User.UserRepo where

import Protolude
import Feature.User.User (User)
import Database.PostgreSQL.Simple
import Feature.User.UserRepoClass (InsertUser(..))
import Infrastructure.Utils.Id (Id)
import Infrastructure.DB (MonadDB, withConn)

insertUser :: (MonadDB m) => InsertUser -> m ()
insertUser (InsertUser insertUserUsername insertUserEmail insertUserPassword) =
  withConn $ \conn -> do
    let qry = "INSERT INTO users (username, email, password) VALUES (?, ?, ?)"

    void $ execute
      conn
      qry
      (insertUserUsername, insertUserEmail, insertUserPassword)

deleteUser :: (MonadDB m) => Id User -> m ()
deleteUser userId = withConn $ \conn -> do
  let qry = "DELETE FROM users WHERE id = ?"

  void $ execute conn qry (Only userId)

findUserByUsername :: MonadDB m => Text -> m (Maybe User)
findUserByUsername username = withConn $ \conn -> do
  let qry = "SELECT * FROM users WHERE username = ? LIMIT 1"

  results <- liftIO $ query conn qry (Only username)
  return $ head results

findUserByEmail :: MonadDB m => Text -> m (Maybe User)
findUserByEmail email = withConn $ \conn -> do
  let qry = "SELECT * FROM users WHERE email = ? LIMIT 1"

  results <- liftIO $ query conn qry (Only email)
  return $ head results
