module Routes.Auth.Register (register) where

import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.Text                  as T
import qualified Data.Time.Clock            as Time
import           Database.MongoDB           (Action, Pipe, Value, count, find,
                                             insert, rest, select, (=:))
import           GHC.Generics
import           Infrastructure.Maybe       (maybeToEither)
import qualified Models.User                as User
import qualified Network.HTTP.Types.Status  as Status
import           Protolude                  hiding (ByteString, find,
                                             maybeToEither)
import           Web.Scotty                 (ActionM, body, json, status)

instance ToJSON RegisterBody
instance FromJSON RegisterBody
data RegisterBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic

instance ToJSON RegisterError
data RegisterError
    = ValidationFailed
    | UserAlreadyExists
    | BadRequest
    deriving Generic

instance ToJSON RegisterResponse
data RegisterResponse = RegisterResponse
    { errorDescription :: RegisterError
    } deriving Generic

parseBody :: ByteString -> Either RegisterError RegisterBody
parseBody b =
    maybeToEither BadRequest $ decode b

mkUser :: Time.UTCTime -> RegisterBody -> Either RegisterError User.User
mkUser dateNow req =
    maybeToEither ValidationFailed $ User.User
        <$> (User.mkUsername $ username req)
        <*> (User.mkPassword $ password req)
        <*> pure dateNow

insertUser :: User.User -> Action IO Value
insertUser user =
    insert "user"
        [ "username" =: (User.unwrapUsername $ User.username user)
        , "password" =: (User.unwrapPassword $ User.password user)
        , "createdOn" =: User.createdOn user
        ]

doesUserExist :: Text -> Action IO Bool
doesUserExist username =
    (== 1) <$> count (select ["username" =: username] "user")

insertUserWithCheck :: User.User -> Action IO (Either RegisterError Value)
insertUserWithCheck user = do
    userExists <- doesUserExist $ User.unwrapUsername $ User.username user

    if userExists then do
        doc <- insertUser user
        pure $ Right doc
    else
        pure $ Left UserAlreadyExists


register :: Pipe -> ActionM ()
register _ = do
    now <- liftIO Time.getCurrentTime
    rawBody <- body

    let a = parseBody rawBody >>= mkUser now
    let b = insertUserWithCheck <$> a

    status Status.badRequest400
