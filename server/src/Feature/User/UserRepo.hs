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
insertUser (InsertUser insertUserUsername insertUserEmail insertUserPassword) =
  withConn $ \conn -> do
    let qry = "INSERT INTO users (username, email, password) VALUES (?, ?, ?)"

    void $ execute
      conn
      qry
      (insertUserUsername, insertUserEmail, insertUserPassword)

findUser :: MonadDB m => Text -> m (Maybe User)
findUser username = withConn $ \conn -> do
  let
    qry =
      "SELECT * FROM users\
        \WHERE username = ?\
        \LIMIT 1"

  results <- liftIO $ query conn qry (Only username)
  return $ head results
