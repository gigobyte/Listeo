module Feature.User.HTTP
    ( register
    ) where

import           Data.Aeson                (decode)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.Time.Clock           as Time
import           Database.MongoDB          as DB
import           Feature.User.Service      as Service
import           Feature.User.Types        (RegisterBody (..),
                                            RegisterError (..),
                                            RegisterResponse (..))
import           Infrastructure.DB         as DB
import           Infrastructure.Maybe      (maybeToEither)
import qualified Network.HTTP.Types.Status as Status
import           Protolude                 hiding (ByteString, maybeToEither)
import           Web.Scotty                (ActionM, body, json, status)

toHttpResult :: Either RegisterError a -> ActionM ()
toHttpResult (Left ServerError)      = status Status.internalServerError500
toHttpResult (Left ValidationFailed) = status Status.badRequest400
toHttpResult (Left err)              = json $ RegisterResponse err
toHttpResult _                       = status Status.ok200

parseBody :: ByteString -> Either RegisterError RegisterBody
parseBody b =
    maybeToEither ValidationFailed $ decode b

register :: DB.Pipe -> ActionM ()
register pipe = do
    dateNow <- liftIO Time.getCurrentTime

    result <- liftIO
            . DB.runQuery pipe
            . (Service.insertUser <$>)
            . (Service.mkUser dateNow =<<)
            . parseBody
            =<< body

    toHttpResult result
