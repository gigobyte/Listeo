module Infrastructure.DB
  ( runQuery
  , withConn
  , MonadDB
  , Env
  )
where

import Protolude
import Database.MongoDB (Action, Pipe, access, master)

type Connection = Pipe
type Env = Connection

type MonadDB r m = (MonadReader Connection m, MonadIO m)

withConn :: MonadDB r m => (Connection -> IO a) -> m a
withConn action = do
  conn <- ask
  liftIO $ action conn

runQuery :: Connection -> Action IO a -> IO a
runQuery pipe = access pipe master "listeodb"
