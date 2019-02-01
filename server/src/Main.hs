module Main where

import Protolude hiding (get)
import Web.Scotty.Trans
import Infrastructure.MonadTime
import Infrastructure.MonadCrypto
import Feature.Login.LoginServiceClass (LoginService(..))
import Feature.Register.RegisterServiceClass (RegisterService(..))
import Feature.User.UserRepoClass (UserRepo(..))
import qualified Database.MongoDB as DB
import qualified Feature.Login.LoginHTTP as LoginHTTP
import qualified Feature.Login.LoginService as LoginService
import qualified Feature.Register.RegisterHTTP as RegisterHTTP
import qualified Feature.Register.RegisterService as RegisterService
import qualified Feature.User.UserHTTP as User
import qualified Feature.User.UserRepo as UserRepo
import qualified Infrastructure.DB as DB
import qualified Infrastructure.Utils.Crypto as Crypto
import qualified Data.Time.Clock as Time
import qualified Infrastructure.Middleware.Cors as Middleware

type Env = (DB.Env)

type App m = (MonadIO m, LoginService m, RegisterService m, UserRepo m)

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

routes :: App m => ScottyT LText m ()
routes = do
  RegisterHTTP.routes
  LoginHTTP.routes
  User.routes

  get "/health" $ json True

main :: IO ()
main = do
  pipe <- DB.connect $ DB.host "127.0.0.1"
  scottyT 8081 (\app -> flip runReaderT (pipe) $ unAppT app) $ do
    Middleware.cors
    routes

instance MonadTime AppT where
  getCurrentTime = liftIO Time.getCurrentTime

instance MonadCrypto AppT where
  hash = Crypto.hash

instance UserRepo AppT where
  insertUser = UserRepo.insertUser
  findUser = UserRepo.findUser

instance RegisterService AppT where
  register = RegisterService.register

instance LoginService AppT where
  login = LoginService.login
