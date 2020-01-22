module Feature.Login.LoginServiceClass where

import Protolude
import Feature.Login.LoginResult

class Monad m => LoginService m where
  login :: LByteString -> m (Either LoginError Text)

