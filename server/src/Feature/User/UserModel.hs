module Feature.User.UserModel
    ( Username
    , Password
    , unUsername
    , unPassword
    , mkUsername
    , mkPassword
    , hashPassword
    , User(..)
    ) where

import qualified Data.Text             as T
import qualified Data.Time.Clock       as Time
import qualified Infrastructure.Crypto as Crypto
import           Protolude

newtype Username =
    Username { unUsername :: Text }

newtype Password =
    Password { unPassword :: Text }

mkUsername :: Text -> Maybe Username
mkUsername str
    | T.length str < 4 = Nothing
    | otherwise = Just $ Username $ T.strip str

mkPassword :: Text -> Maybe Password
mkPassword str
    | T.length str < 6 = Nothing
    | otherwise = Just $ Password $ T.strip str

hashPassword :: Password -> IO (Maybe Password)
hashPassword password =
    mapToPassword <$> (Crypto.hash $ encodeUtf8 $ unPassword $ password)
        where
            mapToPassword :: Maybe ByteString -> Maybe Password
            mapToPassword maybePass = Password <$> decodeUtf8 <$> maybePass

data User = User
    { username  :: Username
    , password  :: Password
    , createdOn :: Time.UTCTime
    }
