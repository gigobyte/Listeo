module Feature.Login.HTTP
    ( login
    ) where

import           Data.Aeson            (decode)
import           Data.ByteString.Lazy  (ByteString)
import qualified Database.MongoDB      as DB
import qualified Feature.Login.Service as Service
import           Feature.Login.Types   (LoginBody, LoginError (..),
                                        LoginResponse (..))
import qualified Infrastructure.DB     as DB
import           Protolude             hiding (ByteString)
import           Web.Scotty            (ActionM, body, json)

toHttpResult :: Either LoginError Text -> ActionM ()
toHttpResult (Left err)       = json $ ErrorResponse err
toHttpResult (Right jwtToken) = json $ SuccessResponse jwtToken

parseBody :: ByteString -> Either LoginError LoginBody
parseBody rawBody =
    maybeToRight ValidationFailed (decode rawBody)

login :: DB.Pipe -> ActionM ()
login pipe = do
    parsedBody <- parseBody <$> body

    let query = Service.findUserByCredentials <$> parsedBody
    let flatQuery = join <$> sequence query

    result <- liftIO $ DB.runQuery pipe flatQuery

    let jwtToken = Service.generateJwtToken <$> result

    toHttpResult jwtToken
