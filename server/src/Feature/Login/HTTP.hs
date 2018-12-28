module Feature.Login.HTTP
  ( login
  )
where

import qualified Data.Aeson as Aeson
import qualified Database.MongoDB as DB
import qualified Feature.Login.Service as Service
import Feature.Login.Models.LoginBody (LoginBody)
import Feature.Login.Models.LoginResponse (LoginResponse(..), LoginError(..))
import qualified Infrastructure.DB as DB
import Protolude
import qualified Web.Scotty as Scotty

toHttpResult :: Either LoginError Text -> ActionM ()
toHttpResult (Left  err     ) = json $ ErrorResponse err
toHttpResult (Right jwtToken) = json $ SuccessResponse jwtToken

parseBody :: LByteString -> Either LoginError LoginBody
parseBody rawBody = maybeToRight ValidationFailed (Aeson.decode rawBody)

login :: DB.Pipe -> ActionM ()
login pipe = do
  parsedBody <- parseBody <$> body

  let query     = Service.findUserByCredentials <$> parsedBody
  let flatQuery = join <$> sequence query

  result <- liftIO $ DB.runQuery pipe flatQuery

  let jwtToken = Service.generateJwtToken <$> result

  toHttpResult jwtToken
