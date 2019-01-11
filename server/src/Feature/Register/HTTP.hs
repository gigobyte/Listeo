module Feature.Register.HTTP
  ( register
  )
where

import Protolude
import Flow
import Feature.Register.Models.RegisterBody (RegisterBody(..))
import Feature.Register.Models.RegisterResponse
  (RegisterError(..), RegisterResponse(..))
import Control.Monad.Except (liftEither)
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time
import qualified Database.MongoDB as DB
import qualified Feature.Register.Service as Service
import qualified Web.Scotty as Scotty

toHttpResult :: Either RegisterError a -> Scotty.ActionM ()
toHttpResult (Left err) = Scotty.json <| RegisterResponse (Just err)
toHttpResult _          = Scotty.json <| RegisterResponse Nothing

parseBody :: LByteString -> Either RegisterError RegisterBody
parseBody body = maybeToRight ValidationFailed (Aeson.decode body)

register :: DB.Pipe -> Scotty.ActionM ()
register pipe = do
  -- dateNow    <- liftIO Time.getCurrentTime
  -- parsedBody <- parseBody <$> Scotty.body

  -- let user      = Service.mkUser dateNow =<< parsedBody
  -- let query     = Service.insertUser <$> user
  -- let flatQuery = join <$> sequence query

  -- result <- liftIO (DB.runQuery pipe flatQuery)

  -- toHttpResult result
  body   <- Scotty.body
  result <- liftIO $ register2 pipe body

  toHttpResult result

register2 :: DB.Pipe -> LByteString -> IO (Either RegisterError ())
register2 pipe rawBody = runExceptT $ do
  dateNow <- liftIO Time.getCurrentTime
  body    <- liftEither $ parseBody rawBody
  user    <- liftEither $ Service.mkUser dateNow body

  ExceptT $ Service.insertUser pipe user
