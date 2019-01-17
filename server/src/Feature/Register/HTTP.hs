module Feature.Register.HTTP
  ( routes
  , Service(..)
  )
where

import Protolude
import Web.Scotty.Trans (post, ScottyT, ActionT)
import Feature.Register.Service (RegisterError)
import qualified Web.Scotty.Trans as ScottyT
import qualified Data.Aeson as Aeson

instance Aeson.ToJSON RegisterResponse
data RegisterResponse = RegisterResponse
    { errorDescription :: Maybe RegisterError
    } deriving Generic

class Monad m => Service m where
  register :: LByteString -> m (Either RegisterError ())

toHttpResult :: (Monad m) => Either RegisterError () -> ActionT LText m ()
toHttpResult (Left err) = ScottyT.json $ RegisterResponse (Just err)
toHttpResult _          = ScottyT.json $ RegisterResponse Nothing

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do
  post "/register" $ do
    body   <- ScottyT.body
    result <- lift $ register body

    toHttpResult result
