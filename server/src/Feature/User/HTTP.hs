module Feature.User.HTTP
  ( me
  )
where

import qualified Database.MongoDB              as DB
import           Feature.User.Types             ( PublicUser )
import           Web.Scotty                     ( ActionM
                                                , json
                                                )

me :: DB.Pipe -> PublicUser -> ActionM ()
me _ user = do
  json user
