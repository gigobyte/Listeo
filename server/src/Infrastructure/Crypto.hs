module Infrastructure.Crypto
  ( hash
  , validate
  )
where

import           Crypto.BCrypt                  ( HashingPolicy(..)
                                                , hashPasswordUsingPolicy
                                                , validatePassword
                                                )
import           Data.ByteString.Char8          ( pack )
import           Protolude               hiding ( hash )

hashingPolicy :: HashingPolicy
hashingPolicy = HashingPolicy 12 $ pack "$2y$"

hash :: ByteString -> IO (Maybe ByteString)
hash = hashPasswordUsingPolicy hashingPolicy


validate :: Text -> Text -> Bool
validate hashed attempt =
  validatePassword (encodeUtf8 hashed) (encodeUtf8 attempt)
