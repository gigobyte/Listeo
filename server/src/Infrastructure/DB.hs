module Infrastructure.DB
    ( runQuery
    , joinQuery
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           Database.MongoDB       (MongoContext, Pipe, access, master)
import           Protolude

joinQuery :: (Monad f, Monad m, Traversable m) => m (f (m a)) -> f (m a)
joinQuery q =
    join <$> sequence q

runQuery :: (Traversable m, Monad m, MonadIO m1) => Pipe -> m (ReaderT MongoContext m1 (m a)) -> m1 (m a)
runQuery pipe =
    access pipe master "listeodb" . joinQuery
