module Feature.Login.HTTP
    ( login
    ) where

import           Data.Aeson                (decode)
import           Data.ByteString.Lazy      (ByteString)
import qualified Database.MongoDB          as DB
import           Feature.Login.Types       (LoginBody, LoginError (..),
                                            LoginResponse (..))
import qualified Network.HTTP.Types.Status as Status
import           Protolude                 hiding (ByteString)
import           Web.Scotty                (ActionM, body, json, status)

toHttpResult :: Either LoginError Text -> ActionM ()
toHttpResult (Left err)       = json $ ErrorResponse err
toHttpResult (Right jwtToken) = json $ SuccessResponse jwtToken


parseBody :: ByteString -> Either LoginError LoginBody
parseBody b =
    maybeToRight ValidationFailed $ decode b

login :: DB.Pipe -> ActionM ()
login _ = do
    requestBody <- parseBody <$> body

    status Status.ok200
