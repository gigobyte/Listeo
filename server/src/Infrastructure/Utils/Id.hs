module Infrastructure.Utils.Id
  ( Id
  , getIdFromParam
  )
where

import Protolude
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

type Id a = Int64

getIdFromParam :: LByteString -> Maybe (Id a)
getIdFromParam = readMaybe . T.unpack . decodeUtf8 . B.toStrict
