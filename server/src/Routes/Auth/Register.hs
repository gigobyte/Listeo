module Routes.Auth.Register (register) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Aeson                (decode)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.Text                 as T
import           Data.Time.Clock           (getCurrentTime)
import           Database.MongoDB          (Pipe)
import           Database.MongoDB          (Action, Value, insert, (=:))
import           GHC.Generics
import qualified Models.User               as User
import qualified Network.HTTP.Types.Status as Status
import           Protolude                 hiding (ByteString)
import           Web.Scotty                (ActionM, body, json, status)

instance ToJSON RegisterBody
instance FromJSON RegisterBody
data RegisterBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic

instance ToJSON RegisterValidationError
data RegisterValidationError
    = ValidationFailed
    | UserAlreadyExists
    deriving Generic

instance ToJSON RegisterResponse
data RegisterResponse = RegisterResponse
    { errorDescription :: RegisterValidationError
    } deriving Generic

parseBody :: ByteString -> Maybe RegisterBody
parseBody =
    decode

createUser :: RegisterBody -> Maybe User.User
createUser req =
    User.User
        <$> (User.mkUsername $ username req)
        <*> (User.mkPassword $ password req)

insertUser :: User.User -> Action IO Value
insertUser user =
    insert "user"
        [ "username" =: (T.unpack $ User.unwrapUsername $ User.username user)
        , "password" =: (T.unpack $ User.unwrapPassword $ User.password user)
        ]

register :: Pipe -> ActionM ()
register _ = do
    b <- body
    now <- liftIO $ getCurrentTime

    let request = parseBody b

    status Status.badRequest400
