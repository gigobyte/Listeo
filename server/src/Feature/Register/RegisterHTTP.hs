module Feature.Register.RegisterHTTP
  ( routes
  )
where

import Protolude
import Web.Scotty.Trans (post, ScottyT)
import Feature.Register.RegisterServiceClass (RegisterService(..))
import qualified Feature.Register.RegisterResult as RegisterResult
import qualified Web.Scotty.Trans as ScottyT

routes :: (RegisterService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/register" $ do
    body   <- ScottyT.body
    result <- lift $ register body
    RegisterResult.toHttpResult result
