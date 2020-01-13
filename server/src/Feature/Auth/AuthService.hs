module Feature.Auth.AuthService where

import Protolude
import Web.Scotty.Trans (ActionT, header, status, finish)
import Control.Monad.Trans.Maybe
import Network.HTTP.Types.Status (status401)
import Feature.User.UserRepoClass (UserRepo, findUser)
import Feature.User.User (User)
import Infrastructure.Utils.Maybe (liftMaybe)
import qualified Infrastructure.Utils.JWT as JWT

optionalUser :: (UserRepo m) => ActionT LText m (Maybe User)
optionalUser = do
  maybeAuthHeader <- header "Authorization"

  lift $ runMaybeT $ do
    authHeader <- liftMaybe maybeAuthHeader
    subject    <- liftMaybe $ JWT.subjectFromHeader authHeader
    MaybeT $ findUser subject

requireUser :: (UserRepo m) => ActionT LText m User
requireUser = do
  maybeUser <- optionalUser

  case maybeUser of
    Just u  -> pure u
    Nothing -> do
      status status401
      finish
