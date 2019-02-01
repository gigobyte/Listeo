module Feature.Register.RegisterHTTP
  ( routes
  )
where

import Protolude
import Infrastructure.AppError
import Web.Scotty.Trans (post, ScottyT, ActionT)
import Feature.Register.RegisterError (RegisterError)
import Feature.Register.RegisterResponse (RegisterResponse(..))
import Feature.Register.RegisterServiceClass (RegisterService(..))
import qualified Web.Scotty.Trans as ScottyT

toHttpResult
  :: Monad m => Either (AppError RegisterError) () -> ActionT LText m ()
toHttpResult (Left err) = ScottyT.json $ RegisterResponse (Just err)
toHttpResult _          = ScottyT.json $ RegisterResponse Nothing

routes :: (RegisterService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/register" $ do
    body   <- ScottyT.body
    result <- lift $ register body

    toHttpResult result
