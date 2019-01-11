module Feature.User.DB
  ( findUser
  , insertUser
  , doesUserAlreadyExist
  )
where

import Protolude
import Database.MongoDB (Pipe, findOne, insert, select, (=:))
import Feature.User.Models.User (User)
import Infrastructure.DB (runQuery)
import qualified Feature.User.Models.User as User

insertUser :: Pipe -> User.User -> IO ()
insertUser pipe user = runQuery pipe (void $ insert "user" (User.toBson user))

findUser :: Pipe -> Text -> IO (Maybe User)
findUser pipe username = do
  maybeUser <- runQuery pipe $ findOne (select ["username" =: username] "user")
  pure $ User.fromBson =<< maybeUser

doesUserAlreadyExist :: Pipe -> User -> IO Bool
doesUserAlreadyExist pipe user = do
  userInDB <- findUser pipe (User.username user)
  case userInDB of
    Just _  -> return True
    Nothing -> return False
