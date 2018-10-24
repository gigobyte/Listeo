module Routes.Auth.Register (register) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Aeson                (decode)
import           Database.MongoDB          (Pipe)
import           GHC.Generics
import qualified Network.HTTP.Types.Status as Status
import           Protolude
import           Web.Scotty                (ActionM, body, status)

instance ToJSON RegisterBody
instance FromJSON RegisterBody

data RegisterBody = RegisterBody
    { username :: Text
    , password :: Text
    } deriving Generic

register :: Pipe -> ActionM ()
register _ = do
    b <- body

    case (decode b :: Maybe RegisterBody) of
        Just _  -> status Status.ok200
        Nothing -> status Status.badRequest400
