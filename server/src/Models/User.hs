module Models.User (User(..), Username, Password, mkUsername, mkPassword, unwrapUsername, unwrapPassword) where

import qualified Data.Text       as T
import qualified Data.Time.Clock as Time
import           Protolude

newtype Username =
    Username { unwrapUsername :: Text }

newtype Password =
    Password { unwrapPassword :: Text }

mkUsername :: Text -> Maybe Username
mkUsername str
    | T.length str > 4 = Just $ Username str
    | otherwise = Nothing

mkPassword :: Text -> Maybe Password
mkPassword str
    | T.length str > 6 = Just $ Password str
    | otherwise = Nothing

data User = User
    { username  :: Username
    , password  :: Password
    , createdOn :: Time.UTCTime
    }
