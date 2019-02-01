module Feature.User.UserRepo
  ( findUser
  , insertUser
  , doesUserAlreadyExist
  )
where

import Protolude
import Database.MongoDB (findOne, insert_, select, (=:))
import Feature.User.UserDTO (UserDTO)
import Feature.User.User (User)
import qualified Feature.User.User as User
import qualified Feature.User.UserDTO as UserDTO
import Infrastructure.DB (MonadDB, runQuery, withConn)
import Infrastructure.MonadTime

insertUser :: (MonadDB m, MonadTime m) => UserDTO -> m ()
insertUser user = do
  dateNow <- getCurrentTime
  withConn
    $ \conn -> runQuery conn (insert_ "user" (UserDTO.toBson user dateNow))

findUser :: MonadDB m => Text -> m (Maybe User)
findUser username = withConn $ \conn -> do
  maybeUser <- runQuery conn $ findOne (select ["username" =: username] "user")
  return $ User.fromBson =<< maybeUser

doesUserAlreadyExist :: MonadDB m => Text -> m Bool
doesUserAlreadyExist username = do
  userInDB <- findUser username
  return $ isJust userInDB

