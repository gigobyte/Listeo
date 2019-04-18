module AppMock where

import Protolude hiding (get)
import System.IO.Unsafe (unsafePerformIO)
import Database.MongoDB (genObjectId)
import Infrastructure.MonadTime
import Infrastructure.MonadCrypto
import Infrastructure.Utils.Id
import Feature.Login.LoginServiceClass (LoginService(..))
import Feature.Register.RegisterServiceClass (RegisterService(..))
import Feature.Auth.AuthServiceClass (AuthService(..))
import Feature.User.UserRepoClass (UserRepo(..))
import Feature.User.User (User(..))
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import qualified Infrastructure.DB as DB
import qualified Infrastructure.Utils.Crypto as Crypto
import qualified Data.Time.Clock as Time

type Env = (DB.Env)

newtype AppMockT a = AppMockT
    { unAppMockT :: ReaderT Env IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

instance MonadTime AppMockT where
    getCurrentTime = liftIO Time.getCurrentTime

instance MonadCrypto AppMockT where
    hash = Crypto.hash

instance UserRepo AppMockT where
    insertUser = \_ -> pure ()
    findUser = \_ -> pure $ Just mockUser

instance AuthService AppMockT where
    requireUser = pure mockUser

instance RegisterService AppMockT where
    register = \_ -> pure $ Right ()

instance LoginService AppMockT where
    login = \_ -> pure $ Right "jwt token lol"

instance PlaylistService AppMockT where
    createPlaylist = \_ -> pure $ Right mockId
    getPlaylist = undefined

instance PlaylistRepo AppMockT where
    insertPlaylist = \_ -> pure mockId
    findPlaylist = undefined

instance PlaylistTagRepo AppMockT where
    insertPlaylistTag = \_ _ -> pure ()

mockId :: Id a
mockId = Id $ unsafePerformIO genObjectId

mockUser :: User
mockUser = User
  { id        = mockId
  , username  = "mockUser"
  , password  = "mockUser"
  , createdOn = unsafePerformIO $ Time.getCurrentTime
  }
