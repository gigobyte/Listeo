module Feature.Login.LoginResult where

import Protolude
import Data.Aeson (ToJSON)
import Web.Scotty.Trans
import Network.HTTP.Types.Status (badRequest400, status500)
import Infrastructure.AppError (ErrorResponse(..))

instance ToJSON LoginError
data LoginError
  = UserNotFound
  | InvalidRequest
  deriving Generic

instance ToJSON LoginResponse
data LoginResponse
  = LoginResponse { jwt :: Text }
  deriving Generic

toHttpResult :: Monad m => Either LoginError Text -> ActionT LText m ()
toHttpResult (Left InvalidRequest) = do
  status status500
  finish
toHttpResult (error@(Left UserNotFound)) = do
  status badRequest400
  json $ ErrorResponse error
toHttpResult (Right jwt) = json $ LoginResponse jwt
