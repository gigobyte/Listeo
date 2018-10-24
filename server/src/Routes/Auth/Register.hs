module Routes.Auth.Register (register) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Aeson                (decode)
import qualified Data.Text                 as T
import           Database.MongoDB          (Pipe)
import           GHC.Generics
import qualified Network.HTTP.Types.Status as Status
import           Protolude
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

validateRegisterBody :: RegisterBody -> Either RegisterValidationError RegisterBody
validateRegisterBody model =
    if (T.length $ T.strip $ username model) < 4 then
        Left ValidationFailed
    else if (T.length $ T.strip $ password model) < 6 then
        Left ValidationFailed
    else
        Right model

register :: Pipe -> ActionM ()
register _ = do
    b <- body

    case (decode b :: Maybe RegisterBody) of
        Just rawBody  -> case validateRegisterBody rawBody of
            Left err -> json $ RegisterResponse { errorDescription = err }
            Right _  -> status Status.ok200
        Nothing -> status Status.badRequest400
