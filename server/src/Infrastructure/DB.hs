module Infrastructure.DB
  ( withConn
  , init
  , MonadDB
  , Env
  )
where

import Protolude
import qualified Data.Text as T
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import System.Environment
import Infrastructure.Secrets (dbPassword)

type Env = Pool Connection

type MonadDB m = (MonadReader Env m, MonadIO m)

init :: IO Env
init = do
  pool <- acquirePool
  migrateDb pool
  return pool

acquirePool :: IO (Pool Connection)
acquirePool = do
  envUrl <- lookupEnv "DATABASE_URL"
  let
    pgUrl = toS $ fromMaybe
      ("postgresql://postgres:" <> dbPassword <> "@localhost/listeo")
      (T.pack <$> envUrl)
  createPool (connectPostgreSQL pgUrl) close 1 10 10

migrateDb :: Pool Connection -> IO ()
migrateDb pool = withResource pool
  $ \conn -> void $ withTransaction conn (runMigration (ctx conn))
 where
  ctx = MigrationContext cmd False
  cmd =
    MigrationCommands [MigrationInitialization, MigrationDirectory "postgresql"]

withConn :: MonadDB m => (Connection -> IO a) -> m a
withConn action = do
  pool <- ask
  liftIO $ withResource pool action
