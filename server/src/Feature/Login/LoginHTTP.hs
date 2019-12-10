module Feature.Login.LoginHTTP
  ( routes
  )
where

import Protolude
import qualified Feature.Login.LoginResult as LoginResult
import Feature.Login.LoginServiceClass (LoginService(..))
import Web.Scotty.Trans (post, ScottyT)
import qualified Web.Scotty.Trans as ScottyT

routes :: (LoginService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/login" $ do
    rawBody <- ScottyT.body
    result  <- lift $ login rawBody
    LoginResult.toHttpResult result
