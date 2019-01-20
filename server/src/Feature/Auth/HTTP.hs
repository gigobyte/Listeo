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
import Infrastructure.Utils (liftMaybe)
import qualified Web.JWT as JWT

requireUser :: (Monad m) => ActionT LText m Text
requireUser = do
  maybeAuthHeader <- ScottyT.header "Authorization"

  maybeUserId     <- runMaybeT $ do
    authHeader    <- liftMaybe maybeAuthHeader
    unverifiedJwt <- liftMaybe $ JWT.decode $ toStrict $ drop 7 $ authHeader
    subject       <- liftMaybe $ JWT.sub $ JWT.claims unverifiedJwt

    return $ JWT.stringOrURIToText subject

  case maybeUserId of
    Just u  -> pure u
    Nothing -> do
      ScottyT.status status401
      ScottyT.finish
