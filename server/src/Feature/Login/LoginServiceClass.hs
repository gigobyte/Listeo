module Feature.Login.LoginServiceClass where

import Protolude
import Feature.Login.LoginError (LoginError)

class Monad m => LoginService m where
    login :: LByteString -> m (Either LoginError Text)

