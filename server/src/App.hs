module App where

import Protolude hiding (get)
import Infrastructure.MonadTime
import Infrastructure.MonadCrypto
import Feature.Login.LoginServiceClass (LoginService(..))
import Feature.Register.RegisterServiceClass (RegisterService(..))
import Feature.User.UserRepoClass (UserRepo(..))
import Feature.Auth.AuthServiceClass (AuthService(..))
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import qualified Feature.Login.LoginService as LoginService
import qualified Feature.Register.RegisterService as RegisterService
import qualified Feature.Auth.AuthService as AuthService
import qualified Feature.User.UserRepo as UserRepo
import qualified Feature.Playlist.CreatePlaylist.CreatePlaylistService as PlaylistService
import qualified Feature.Playlist.GetPlaylist.GetPlaylistService as PlaylistService
import qualified Feature.Playlist.PlaylistRepo as PlaylistRepo
import qualified Feature.PlaylistTag.PlaylistTagRepo as PlaylistTagRepo
import qualified Infrastructure.DB as DB
import qualified Infrastructure.Utils.Crypto as Crypto
import qualified Data.Time.Clock as Time

type Env = (DB.Env)

newtype AppT a = AppT
    { unAppT :: ReaderT Env IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

instance MonadTime AppT where
    getCurrentTime = liftIO Time.getCurrentTime

instance MonadCrypto AppT where
    hash = Crypto.hash

instance UserRepo AppT where
    insertUser = UserRepo.insertUser
    findUser = UserRepo.findUser

instance AuthService AppT where
    requireUser = AuthService.requireUser

instance RegisterService AppT where
    register = RegisterService.register

instance LoginService AppT where
    login = LoginService.login

instance PlaylistService AppT where
    createPlaylist = PlaylistService.createPlaylist
    getPlaylist = PlaylistService.getPlaylist

instance PlaylistRepo AppT where
    insertPlaylist = PlaylistRepo.insertPlaylist
    findPlaylist = PlaylistRepo.findPlaylist

instance PlaylistTagRepo AppT where
    insertPlaylistTag = PlaylistTagRepo.insertPlaylistTag
    findPlaylistTagsByPlaylist = PlaylistTagRepo.findPlaylistTagsByPlaylist
