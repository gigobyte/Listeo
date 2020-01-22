module Feature.Login.LoginHTTP
  ( routes
  )
where

import Protolude
import qualified Feature.Login.LoginResult as LoginResult
import Feature.Login.LoginServiceClass
import Web.Scotty.Trans

routes :: (LoginService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/login" $ do
    rawBody <- body
    result  <- lift $ login rawBody
    LoginResult.toHttpResult result
