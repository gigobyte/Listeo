module Feature.Login.LoginHTTP
  ( routes
  )
where

import Protolude
import Infrastructure.AppError (ErrorResponse(..))
import Feature.Login.LoginError (LoginError)
import Network.HTTP.Types.Status (badRequest400)
import Feature.Login.LoginResponse (LoginResponse(..))
import Feature.Login.LoginServiceClass (LoginService(..))
import Web.Scotty.Trans (post, ScottyT, ActionT)
import qualified Web.Scotty.Trans as ScottyT

toHttpResult :: Monad m => Either LoginError Text -> ActionT LText m ()
toHttpResult (Left error) = do
  ScottyT.status badRequest400
  ScottyT.json $ ErrorResponse error
toHttpResult (Right jwt) = ScottyT.json $ LoginResponse jwt

routes :: (LoginService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/login" $ do
    rawBody <- ScottyT.body
    result  <- lift $ login rawBody

    toHttpResult $ result
