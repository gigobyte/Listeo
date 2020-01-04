module Feature.Register.RegisterResult where

import Protolude
import Data.Aeson (ToJSON)
import Infrastructure.AppError
import Network.HTTP.Types.Status (badRequest400, status500)
import Web.Scotty.Trans

instance ToJSON RegisterError where
data RegisterError
  = UserAlreadyExists
  | InvalidRequest
  | PasswordHashingFailed
  | ValidationFailed
  deriving Generic

instance ToJSON RegisterResponse
data RegisterResponse
  = RegisterResponse { jwt :: Text }
  deriving Generic

toHttpResult :: Monad m => Either RegisterError Text -> ActionT LText m ()
toHttpResult (Left UserAlreadyExists) = do
  status badRequest400
  json $ ErrorResponse UserAlreadyExists
toHttpResult (Left InvalidRequest) = do
  status status500
  finish
toHttpResult (Left ValidationFailed) = do
  status status500
  finish
toHttpResult (Left PasswordHashingFailed) = do
  status status500
  finish
toHttpResult (Right jwt) = json $ RegisterResponse jwt
