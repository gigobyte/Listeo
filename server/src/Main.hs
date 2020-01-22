module Main where

import Protolude hiding (get)
import qualified App
import qualified AppMock
import System.Environment (lookupEnv)
import Web.Scotty.Trans
import Infrastructure.AppError (ErrorResponse(..))
import Network.HTTP.Types.Status (status500)
import Feature.Login.LoginServiceClass (LoginService)
import Feature.Register.RegisterServiceClass (RegisterService)
import Feature.Auth.AuthServiceClass (AuthService)
import Feature.Playlist.PlaylistServiceClass (PlaylistService)
import Feature.Video.VideoServiceClass (VideoService)
import qualified Feature.Login.LoginHTTP as Login
import qualified Feature.Register.RegisterHTTP as Register
import qualified Feature.User.UserHTTP as User
import qualified Feature.Playlist.PlaylistHTTP as Playlist
import qualified Feature.Video.VideoHTTP as Video
import qualified Infrastructure.Middleware.Cors as Middleware
import qualified Infrastructure.DB as DB

type App m
  = ( MonadIO m
    , LoginService m
    , RegisterService m
    , PlaylistService m
    , AuthService m
    , VideoService m
    )

routes :: App m => ScottyT LText m ()
routes = do
  Middleware.cors

  Register.routes
  Login.routes
  User.routes
  Playlist.routes
  Video.routes

  get "/health" $ json True

  defaultHandler $ \str -> do
    status status500
    json $ ErrorResponse str

isMockEnv :: IO Bool
isMockEnv = do
  isMockVar <- lookupEnv "MOCK"

  case isMockVar of
    Just "true" -> return True
    _           -> return False

main :: IO ()
main = do
  isMock <- isMockEnv
  dbEnv  <- DB.init

  if isMock
    then scottyT
      8081
      (\app -> runReaderT (AppMock.unAppMockT app) (dbEnv))
      routes
    else scottyT 8081 (\app -> runReaderT (App.unAppT app) (dbEnv)) routes
