module Feature.Register.RegisterServiceClass where

import Protolude
import Feature.Register.RegisterResult

class Monad m => RegisterService m where
  register :: LByteString -> m (Either RegisterError Text)

