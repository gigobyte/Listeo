module Main where

import Protolude hiding (get)
import Web.Scotty.Trans
import Infrastructure.MonadTime
import Infrastructure.MonadCrypto
import Feature.Login.LoginServiceClass (LoginService(..))
import Feature.Register.RegisterServiceClass (RegisterService(..))
import Feature.User.UserRepoClass (UserRepo(..))
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import qualified Database.MongoDB as DB
import qualified Feature.Login.LoginHTTP as Login
import qualified Feature.Login.LoginService as LoginService
import qualified Feature.Register.RegisterHTTP as Register
import qualified Feature.Register.RegisterService as RegisterService
import qualified Feature.User.UserHTTP as User
import qualified Feature.User.UserRepo as UserRepo
import qualified Feature.Playlist.PlaylistHTTP as Playlist
import qualified Feature.Playlist.CreatePlaylist.CreatePlaylistService as PlaylistService
import qualified Feature.Playlist.PlaylistRepo as PlaylistRepo
import qualified Feature.PlaylistTag.PlaylistTagRepo as PlaylistTagRepo
import qualified Infrastructure.DB as DB
import qualified Infrastructure.Utils.Crypto as Crypto
import qualified Data.Time.Clock as Time
import qualified Infrastructure.Middleware.Cors as Middleware

type Env = (DB.Env)

type App m = (MonadIO m, LoginService m, RegisterService m, PlaylistService m, UserRepo m)

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

routes :: App m => ScottyT LText m ()
routes = do
  Register.routes
  Login.routes
  User.routes
  Playlist.routes

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

instance PlaylistService AppT where
  createPlaylist = PlaylistService.createPlaylist

instance PlaylistRepo AppT where
  insertPlaylist = PlaylistRepo.insertPlaylist

instance PlaylistTagRepo AppT where
  insertPlaylistTag = PlaylistTagRepo.insertPlaylistTag
