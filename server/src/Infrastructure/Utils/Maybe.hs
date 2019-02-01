module Infrastructure.Utils.Maybe where

import Protolude

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

