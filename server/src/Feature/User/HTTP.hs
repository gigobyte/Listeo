module Feature.User.HTTP
  ( me
  )
where

import Feature.User.Models.PublicUser (PublicUser)
import qualified Database.MongoDB as DB
import qualified Web.Scotty as Scotty

me :: DB.Pipe -> PublicUser -> Scotty.ActionM ()
me _ user = do
  Scotty.json user
