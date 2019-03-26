{-# LANGUAGE AllowAmbiguousTypes #-}

module Infrastructure.MonadCrypto where

import Protolude

class Monad m => MonadCrypto m where
    hash :: ByteString -> m (Maybe ByteString)
