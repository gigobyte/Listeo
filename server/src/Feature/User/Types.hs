module Feature.User.Types
    ( User(..)
    ) where

import qualified Data.Time.Clock as Time
import           Protolude

data User = User
    { username  :: Text
    , password  :: Text
    , createdOn :: Time.UTCTime
    }
