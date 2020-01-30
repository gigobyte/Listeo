module Infrastructure.HTTP
  ( httpGet
  , MonadHTTP
  )
where

import Protolude
import Network.HTTP.Simple
import qualified Data.Text as T

type MonadHTTP m = (MonadIO m)

httpGet :: MonadHTTP m => Text -> m (Response LByteString)
httpGet = httpLBS . parseRequest_ . T.unpack
