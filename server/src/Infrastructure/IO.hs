module Infrastructure.IO where

import Protolude
import qualified Data.Time.Clock as Time

class Monad m => MonadTime m where
    currentTime :: m Time.UTCTime

class Monad m => MonadCrypto m where
    cryptoHash :: ByteString -> m (Maybe ByteString)
