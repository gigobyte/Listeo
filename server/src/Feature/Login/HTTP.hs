module Feature.Login.HTTP
  ( login
  )
where

import Protolude
import Feature.Login.Models.LoginBody (LoginBody)
import Feature.Login.Models.LoginResponse (LoginResponse(..), LoginError(..))
import qualified Data.Aeson as Aeson
import qualified Database.MongoDB as DB
import qualified Feature.Login.Service as Service
import qualified Infrastructure.DB as DB
import qualified Web.Scotty as Scotty

toHttpResult :: Either LoginError Text -> Scotty.ActionM ()
toHttpResult (Left  err     ) = Scotty.json $ ErrorResponse err
toHttpResult (Right jwtToken) = Scotty.json $ SuccessResponse jwtToken

parseBody :: LByteString -> Either LoginError LoginBody
parseBody rawBody = maybeToRight ValidationFailed (Aeson.decode rawBody)

login :: DB.Pipe -> Scotty.ActionM ()
login pipe = do
  parsedBody <- parseBody <$> Scotty.body

  let query     = Service.findUserByCredentials <$> parsedBody
  let flatQuery = join <$> sequence query

  result <- liftIO $ DB.runQuery pipe flatQuery

  let jwtToken = Service.generateJwtToken <$> result

  toHttpResult jwtToken
