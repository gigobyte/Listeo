module Feature.Login.HTTP
  ( login
  )
where

import Protolude
import Feature.Login.Models.LoginResponse (LoginResponse(..), LoginError(..))
import qualified Database.MongoDB as DB
import qualified Feature.Login.Service as Service
import qualified Web.Scotty as Scotty

toHttpResult :: Either LoginError Text -> Scotty.ActionM ()
toHttpResult (Left  error   ) = Scotty.json (ErrorResponse error)
toHttpResult (Right jwtToken) = Scotty.json (SuccessResponse jwtToken)

login :: DB.Pipe -> Scotty.ActionM ()
login pipe = do
  body   <- Scotty.body
  result <- liftIO $ Service.login pipe body

  toHttpResult result
