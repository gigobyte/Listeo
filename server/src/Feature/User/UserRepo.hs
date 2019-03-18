module Feature.User.UserRepo
  ( findUser
  , insertUser
  )
where

import Protolude
import Database.MongoDB (findOne, insert_, select, (=:))
import Feature.User.UserDTO (UserDTO)
import Feature.User.User (User)
import qualified Feature.User.User as User
import qualified Feature.User.UserDTO as UserDTO
import Infrastructure.DB (MonadDB, runQuery, withConn)

insertUser :: (MonadDB m) => UserDTO -> m ()
insertUser user =
  withConn $ \conn -> runQuery conn (insert_ "user" (UserDTO.toBson user))

findUser :: MonadDB m => Text -> m (Maybe User)
findUser username = withConn $ \conn -> do
  maybeUser <- runQuery conn $ findOne (select ["username" =: username] "user")
  return $ User.fromBson =<< maybeUser
