module Infrastructure.Utils.Maybe
  ( liftMaybe
  )
where

import Protolude

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

