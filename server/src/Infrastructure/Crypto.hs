module Infrastructure.Crypto (hash) where

import           Crypto.BCrypt         (HashingPolicy (..),
                                        hashPasswordUsingPolicy)
import           Data.ByteString.Char8 (pack)
import           Protolude

hashingPolicy :: HashingPolicy
hashingPolicy =
    HashingPolicy 12 $ pack "$2y$"

hash :: ByteString -> IO (Maybe ByteString)
hash =
    hashPasswordUsingPolicy hashingPolicy
