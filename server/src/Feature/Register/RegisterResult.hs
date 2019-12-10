module Feature.Register.RegisterResult where

import Protolude
import Data.Aeson
import Infrastructure.AppError
import Network.HTTP.Types.Status (badRequest400)
import Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as ScottyT

instance ToJSON RegisterError where
  toJSON = genericToJSON (defaultOptions { tagSingleConstructors = True })
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
  ScottyT.status badRequest400
  ScottyT.json $ ErrorResponse (Just err)
toHttpResult (Right jwt) = ScottyT.json $ RegisterResponse jwt
