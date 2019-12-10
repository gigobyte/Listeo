module Feature.Login.LoginResult where

import Protolude
import Data.Aeson (ToJSON)
import Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as ScottyT
import Network.HTTP.Types.Status (badRequest400)
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
toHttpResult (Left error) = do
  ScottyT.status badRequest400
  ScottyT.json $ ErrorResponse error
toHttpResult (Right jwt) = ScottyT.json $ LoginResponse jwt
