module Feature.Register.HTTP
    ( register
    ) where

import           Data.Aeson               (decode)
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.Time.Clock          as Time
import qualified Database.MongoDB         as DB
import qualified Feature.Register.Service as Service
import           Feature.Register.Types   (RegisterBody (..),
                                           RegisterError (..),
                                           RegisterResponse (..))
import qualified Infrastructure.DB        as DB
import           Protolude                hiding (ByteString)
import           Web.Scotty               (ActionM, body, json)

toHttpResult :: Either RegisterError a -> ActionM ()
toHttpResult (Left err) = json $ RegisterResponse $ Just err
toHttpResult _          = json $ RegisterResponse Nothing

parseBody :: ByteString -> Either RegisterError RegisterBody
parseBody b =
    maybeToRight ValidationFailed $ decode b

register :: DB.Pipe -> ActionM ()
register pipe = do
    dateNow <- liftIO Time.getCurrentTime
    parsedBody <- parseBody <$> body

    let user = Service.mkUser dateNow =<< parsedBody
    let query = Service.insertUser <$> user
    let flatQuery = join <$> sequence query

    result <- liftIO $ DB.runQuery pipe flatQuery

    toHttpResult result
