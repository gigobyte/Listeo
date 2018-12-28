module Infrastructure.Crypto
  ( hash
  , validate
  )
where

import Protolude hiding (hash)
import Data.ByteString.Char8 (pack)
import qualified Crypto.BCrypt as BCrypt

hashingPolicy :: BCrypt.HashingPolicy
hashingPolicy = BCrypt.HashingPolicy 12 $ pack "$2y$"

hash :: ByteString -> IO (Maybe ByteString)
hash = BCrypt.hashPasswordUsingPolicy hashingPolicy


validate :: Text -> Text -> Bool
validate hashed attempt =
  BCrypt.validatePassword (encodeUtf8 hashed) (encodeUtf8 attempt)
