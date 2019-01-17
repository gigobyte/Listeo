module Infrastructure.Middleware.Auth where

import Protolude hiding (drop)
import Data.Text.Lazy (drop)
import Feature.User.DB (User)
import qualified Database.MongoDB as DB
import qualified Feature.User.DB as DB
import qualified Network.HTTP.Types.Status as Status
import qualified Web.JWT as JWT
import qualified Web.Scotty as Scotty

headerToUsername :: LText -> Maybe Text
headerToUsername authHeader = do
  unverifiedJwt <- JWT.decode $ toStrict $ drop 7 $ authHeader
  subject       <- JWT.sub $ JWT.claims unverifiedJwt

  return $ JWT.stringOrURIToText subject

auth :: (DB.Pipe -> User -> Scotty.ActionM ()) -> DB.Pipe -> Scotty.ActionM ()
auth endpoint pipe = do
  authHeader <- Scotty.header "Authorization"

  let maybeUsername = headerToUsername =<< authHeader

  case maybeUsername of
    Just username -> do
      user <- liftIO $ DB.findUser pipe username

      case user of
        Just u  -> endpoint pipe u
        Nothing -> Scotty.status Status.status401
    Nothing -> Scotty.status Status.status401

