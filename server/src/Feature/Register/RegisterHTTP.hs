module Feature.Register.RegisterHTTP where

import Protolude
import Web.Scotty.Trans
import Feature.Register.RegisterServiceClass
import qualified Feature.Register.RegisterResult as RegisterResult

routes :: (RegisterService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/register" $ do
    rawBody <- body
    result  <- lift $ register rawBody
    RegisterResult.toHttpResult result
