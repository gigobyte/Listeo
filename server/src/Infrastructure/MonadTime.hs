module Infrastructure.MonadTime where

import Protolude
import Data.Time.Clock (UTCTime)

class Monad m => MonadTime m where
    getCurrentTime :: m UTCTime
