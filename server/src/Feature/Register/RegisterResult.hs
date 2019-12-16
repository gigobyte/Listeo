module Feature.Register.RegisterResult where

import Protolude
import Data.Aeson (ToJSON)
import Infrastructure.AppError
import Network.HTTP.Types.Status (badRequest400)
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
toHttpResult (Left err) = do
  status badRequest400
  json $ ErrorResponse (Just err)
toHttpResult (Right jwt) = json $ RegisterResponse jwt
