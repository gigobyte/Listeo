module Main where

import Protolude hiding (get)
import qualified App
import qualified AppMock
import System.Environment (lookupEnv)
import Web.Scotty.Trans
import Feature.Login.LoginServiceClass (LoginService(..))
import Feature.Register.RegisterServiceClass (RegisterService(..))
import Feature.User.UserRepoClass (UserRepo(..))
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import qualified Database.MongoDB as DB
import qualified Feature.Login.LoginHTTP as Login
import qualified Feature.Register.RegisterHTTP as Register
import qualified Feature.User.UserHTTP as User
import qualified Feature.Playlist.PlaylistHTTP as Playlist
import qualified Infrastructure.Middleware.Cors as Middleware

type App m = (MonadIO m, LoginService m, RegisterService m, PlaylistService m, UserRepo m)

routes :: App m => ScottyT LText m ()
routes = do
  Register.routes
  Login.routes
  User.routes
  Playlist.routes

  get "/health" $ json True

isMockEnv :: IO Bool
isMockEnv = do
  isMockVar <- lookupEnv "MOCK"

  case isMockVar of
    Just "true" -> return True
    _           -> return False


main :: IO ()
main = do
  isMock <- isMockEnv
  pipe   <- DB.connect $ DB.host "127.0.0.1"

  if isMock
    then scottyT 8081 (\app -> runReaderT (App.unAppT app) (pipe)) $ do
      Middleware.cors
      routes
    else scottyT 8081 (\app -> runReaderT (AppMock.unAppMockT app) (pipe)) $ do
      Middleware.cors
      routes
