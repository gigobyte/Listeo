module Feature.Login.LoginServiceClass where

import Protolude
import Infrastructure.AppError (AppError)
import Feature.Login.LoginError (LoginError)

class Monad m => LoginService m where
    login :: LByteString -> m (Either (AppError LoginError) Text)

