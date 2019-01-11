module Infrastructure.Middleware.Auth where

import Protolude hiding (drop)
import Data.Text.Lazy (drop)
import Feature.User.Models.User (User)
import Feature.User.Models.PublicUser (PublicUser(..))
import qualified Database.MongoDB as DB
import qualified Feature.User.DB as DB
import qualified Feature.User.Models.User as User
import qualified Network.HTTP.Types.Status as Status
import qualified Web.JWT as JWT
import qualified Web.Scotty as Scotty

headerToUsername :: Text -> Maybe Text
headerToUsername authHeader = do
  unverifiedJwt <- JWT.decode authHeader
  subject       <- JWT.sub $ JWT.claims unverifiedJwt

  return $ JWT.stringOrURIToText subject

dbUserToPublicUser :: User -> PublicUser
dbUserToPublicUser user =
  PublicUser {username = User.username user, createdOn = User.createdOn user}

auth
  :: (DB.Pipe -> PublicUser -> Scotty.ActionM ())
  -> DB.Pipe
  -> Scotty.ActionM ()
auth endpoint pipe = do
  authHeader <- Scotty.header "Authorization"

  let maybeUsername = headerToUsername =<< toStrict <$> drop 7 <$> authHeader

  case maybeUsername of
    Just username -> do
      user <- liftIO $ DB.findUser pipe username

      case user of
        Just u  -> endpoint pipe (dbUserToPublicUser u)
        Nothing -> Scotty.status Status.status401
    Nothing -> Scotty.status Status.status401
