module Feature.Register.RegisterServiceClass where

import Protolude
import Infrastructure.AppError (AppError)
import Feature.Register.RegisterError (RegisterError)

class Monad m => RegisterService m where
    register :: LByteString -> m (Either (AppError RegisterError) ())

