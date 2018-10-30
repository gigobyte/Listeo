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
import qualified Feature.User.UserModel    as User
import           Infrastructure.DB         as DB
import           Infrastructure.Maybe      (maybeToEither)
import qualified Network.HTTP.Types.Status as Status
import           Protolude                 hiding (ByteString, maybeToEither)
import           Web.Scotty                (ActionM, body, json, status)

toHttpResult :: Either RegisterError a -> ActionM ()
toHttpResult (Left err) = json $ RegisterResponse { errorDescription = err }
toHttpResult _          = status Status.ok200

mkUser :: Time.UTCTime -> RegisterBody -> Either RegisterError User.User
mkUser dateNow req =
    maybeToEither ValidationFailed $ User.User
        <$> (User.mkUsername $ username req)
        <*> (User.mkPassword $ password req)
        <*> pure dateNow

parseBody :: ByteString -> Either RegisterError RegisterBody
parseBody b =
    maybeToEither BadRequest $ decode b

register :: DB.Pipe -> ActionM ()
register pipe = do
    dateNow <- liftIO Time.getCurrentTime
    rawBody <- body

    result <- liftIO
            . DB.runQuery pipe
            . (Service.insertUser <$>)
            . (mkUser dateNow =<<)
            . parseBody
            $ rawBody

    toHttpResult result
