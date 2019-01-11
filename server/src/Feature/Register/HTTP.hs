module Feature.Register.HTTP
  ( register
  )
where

import Protolude
import Feature.Register.Models.RegisterResponse
  (RegisterError(..), RegisterResponse(..))
import qualified Database.MongoDB as DB
import qualified Feature.Register.Service as Service
import qualified Web.Scotty as Scotty

toHttpResult :: Either RegisterError a -> Scotty.ActionM ()
toHttpResult (Left err) = Scotty.json $ RegisterResponse (Just err)
toHttpResult _          = Scotty.json $ RegisterResponse Nothing

register :: DB.Pipe -> Scotty.ActionM ()
register pipe = do
  body   <- Scotty.body
  result <- liftIO $ Service.register pipe body

  toHttpResult result
