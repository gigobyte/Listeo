module Infrastructure.DB
  ( runQuery
  , withConn
  , MonadDB
  , Env
  , Bson(..)
  )
where

import Protolude
import Database.MongoDB (Action, Document, Pipe, access, master)

type Connection = Pipe
type Env = Connection

type MonadDB m = (MonadReader Connection m, MonadIO m)

withConn :: MonadDB m => (Connection -> IO a) -> m a
withConn action = do
  conn <- ask
  liftIO $ action conn

runQuery :: Connection -> Action IO a -> IO a
runQuery pipe = access pipe master "listeodb"

class Bson a where
  toBson :: a -> Document
  fromBson :: Document -> Maybe a
