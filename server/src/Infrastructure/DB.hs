module Infrastructure.DB
  ( runQuery
  , withConn
  , MonadDB
  , Env
  )
where

import Protolude
import Data.Has
import Database.MongoDB (Action, Pipe, access, master)

type Connection = Pipe
type Env = Connection

type MonadDB r m = (MonadReader r m, Has Connection r, MonadIO m)

withConn :: MonadDB r m => (Connection -> IO a) -> m a
withConn action = do
  conn <- asks getter
  liftIO $ action conn

runQuery :: Connection -> Action IO a -> IO a
runQuery pipe = access pipe master "listeodb"
