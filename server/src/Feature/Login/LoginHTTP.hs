module Feature.Login.LoginHTTP
  ( routes
  )
where

import Protolude
import Infrastructure.AppError (AppError)
import Feature.Login.LoginError (LoginError)
import Feature.Login.LoginResponse (LoginResponse(..))
import Feature.Login.LoginServiceClass (LoginService(..))
import Web.Scotty.Trans (post, body, json, ScottyT, ActionT)

toHttpResult
  :: Monad m => Either (AppError LoginError) Text -> ActionT LText m ()
toHttpResult (Left  error   ) = json $ ErrorResponse error
toHttpResult (Right jwtToken) = json $ SuccessResponse jwtToken

routes :: (LoginService m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/login" $ do
    rawBody <- body
    result  <- lift $ login rawBody

    toHttpResult $ result
