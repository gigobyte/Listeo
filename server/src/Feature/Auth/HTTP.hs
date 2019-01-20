module Feature.Auth.HTTP
  ( requireUser
  )
where

import Protolude hiding (drop)
import Data.Text.Lazy (drop)
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans as ScottyT
import Control.Monad.Trans.Maybe
import Network.HTTP.Types.Status
import Feature.User.Service (UserRepo, findUser)
import Feature.User.DB (User)
import Infrastructure.Utils (liftMaybe)
import qualified Web.JWT as JWT

requireUser :: (UserRepo m) => ActionT LText m User
requireUser = do
  maybeAuthHeader <- ScottyT.header "Authorization"

  maybeUser       <- lift $ runMaybeT $ do
    authHeader    <- liftMaybe maybeAuthHeader
    unverifiedJwt <- liftMaybe $ JWT.decode $ toStrict $ drop 7 $ authHeader
    subject       <- liftMaybe $ JWT.sub $ JWT.claims unverifiedJwt
    user          <- MaybeT $ findUser $ JWT.stringOrURIToText subject

    return user

  case maybeUser of
    Just u  -> pure u
    Nothing -> do
      ScottyT.status status401
      ScottyT.finish
