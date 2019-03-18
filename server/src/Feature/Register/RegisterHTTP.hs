module Feature.Register.RegisterHTTP
  ( routes
  )
where

import Protolude
import Infrastructure.AppError
import Web.Scotty.Trans (post, ScottyT, ActionT)
import Feature.Register.RegisterError (RegisterError)
import Feature.Register.RegisterServiceClass (RegisterService(..))
import qualified Web.Scotty.Trans as ScottyT

mkRegisterHttpResult
  :: Monad m => Either (AppError RegisterError) () -> ActionT LText m ()
mkRegisterHttpResult (Left err) = ScottyT.json $ ErrorResponse (Just err)
mkRegisterHttpResult _          = ScottyT.json emptyErrorResponse

routes :: (RegisterService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/register" $ do
    body   <- ScottyT.body
    result <- lift $ register body

    mkRegisterHttpResult result
