module Feature.Register.RegisterHTTP
  ( routes
  )
where

import Protolude
import Infrastructure.AppError
import Web.Scotty.Trans (post, ScottyT, ActionT)
import Feature.Register.RegisterError (RegisterError)
import Feature.Register.RegisterServiceClass (RegisterService(..))
import Network.HTTP.Types.Status (ok200, badRequest400)
import qualified Web.Scotty.Trans as ScottyT

mkRegisterHttpResult :: Monad m => Either RegisterError () -> ActionT LText m ()
mkRegisterHttpResult (Left err) = do
  ScottyT.status badRequest400
  ScottyT.json $ ErrorResponse (Just err)
mkRegisterHttpResult _ = ScottyT.status ok200

routes :: (RegisterService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/register" $ do
    body   <- ScottyT.body
    result <- lift $ register body

    mkRegisterHttpResult result
