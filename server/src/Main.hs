module Main where

import Protolude hiding (get)
import Web.Scotty.Trans
import Infrastructure.IO (MonadTime, MonadCrypto)
import qualified Database.MongoDB as DB
import qualified Feature.Login.HTTP as Login
import qualified Feature.Login.Service as LoginService
import qualified Feature.Register.HTTP as Register
import qualified Feature.Register.Service as RegisterService
import qualified Feature.User.HTTP as User
import qualified Feature.User.DB as UserDB
import qualified Feature.User.Service as UserService
import qualified Infrastructure.DB as DB
import qualified Infrastructure.Crypto as Crypto
import qualified Data.Time.Clock as Time
import qualified Infrastructure.Middleware.Cors as Middleware

type Env = (DB.Env)

type App m = (MonadIO m, Login.LoginService m, Register.RegisterService m, UserService.UserRepo m)

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

routes :: App m => ScottyT LText m ()
routes = do
  Register.routes
  Login.routes
  User.routes

  get "/health" $ json True

main :: IO ()
main = do
  pipe <- DB.connect $ DB.host "127.0.0.1"
  scottyT 8081 (\app -> flip runReaderT pipe $ unAppT app) $ do
    Middleware.cors
    routes

instance MonadTime AppT where
  currentTime = liftIO Time.getCurrentTime

instance MonadCrypto AppT where
  cryptoHash = Crypto.hash

instance UserService.UserRepo AppT where
  insertUser = UserDB.insertUser
  doesUserAlreadyExist = UserDB.doesUserAlreadyExist
  findUser = UserDB.findUser

instance Register.RegisterService AppT where
  register = RegisterService.register

instance Login.LoginService AppT where
  login = LoginService.login
