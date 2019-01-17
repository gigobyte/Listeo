module Feature.Login.HTTP
  ( login
  )
where

import Protolude
import Feature.Login.Service (LoginError)
import qualified Database.MongoDB as DB
import qualified Feature.Login.Service as Service
import qualified Web.Scotty as Scotty
import qualified Data.Aeson as Aeson

instance Aeson.ToJSON LoginResponse
data LoginResponse
    = ErrorResponse { errorDescription :: LoginError }
    | SuccessResponse { jwt :: Text }
    deriving Generic

toHttpResult :: LoginResponse -> Scotty.ActionM ()
toHttpResult (SuccessResponse jwtToken) = Scotty.json jwtToken
toHttpResult (ErrorResponse   error   ) = Scotty.json error

toLoginResponse :: Either LoginError Text -> LoginResponse
toLoginResponse (Left  error   ) = ErrorResponse error
toLoginResponse (Right jwtToken) = SuccessResponse jwtToken

login :: DB.Pipe -> Scotty.ActionM ()
login pipe = do
  body   <- Scotty.body
  result <- liftIO $ Service.login pipe body

  toHttpResult $ toLoginResponse $ result
