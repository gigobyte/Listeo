module Feature.Login.HTTP
  ( routes
  , Service(..)
  )
where

import Protolude
import Feature.Login.Service (LoginError)
import Web.Scotty.Trans (post, ScottyT, ActionT)
import qualified Web.Scotty.Trans as ScottyT
import qualified Data.Aeson as Aeson

instance Aeson.ToJSON LoginResponse
data LoginResponse
    = ErrorResponse { errorDescription :: LoginError }
    | SuccessResponse { jwt :: Text }
    deriving Generic

class Monad m => Service m where
  login :: LByteString -> m (Either LoginError Text)

toHttpResult :: (Monad m) => LoginResponse -> ActionT LText m ()
toHttpResult (SuccessResponse jwtToken) = ScottyT.json jwtToken
toHttpResult (ErrorResponse   error   ) = ScottyT.json error

toLoginResponse :: Either LoginError Text -> LoginResponse
toLoginResponse (Left  error   ) = ErrorResponse error
toLoginResponse (Right jwtToken) = SuccessResponse jwtToken

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/login" $ do
    body   <- ScottyT.body
    result <- lift $ login body

    toHttpResult $ toLoginResponse $ result
