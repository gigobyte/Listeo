module AppMock where

import Protolude hiding (get)
import System.IO.Unsafe (unsafePerformIO)
import Infrastructure.MonadTime
import Infrastructure.MonadCrypto
import Infrastructure.Utils.Id
import Feature.Login.LoginServiceClass (LoginService(..))
import Feature.Register.RegisterServiceClass (RegisterService(..))
import Feature.Auth.AuthServiceClass (AuthService(..))
import Feature.User.UserRepoClass (UserRepo(..))
import Feature.User.User (User(..))
import Feature.Playlist.Playlist
  (Playlist(..), PlaylistStyle(..), PlaylistPrivacy(..))
import Feature.Playlist.PlaylistServiceClass (PlaylistService(..))
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import Feature.Playlist.GetPlaylist.GetPlaylistResult (GetPlaylistResponse(..))
import Feature.PlaylistTag.PlaylistTag
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
  insertUser _ = pure ()
  findUser _ = pure $ Just mockUser

instance AuthService AppMockT where
  requireUser = pure mockUser

instance RegisterService AppMockT where
  register _ = pure $ Right ""

instance LoginService AppMockT where
  login _ = pure $ Right "some jwt"

instance PlaylistService AppMockT where
  createPlaylist _ _ = pure $ Right mockId
  getPlaylist _ = pure $ Right mockGetPlaylistResponse

instance PlaylistRepo AppMockT where
  insertPlaylist _ = pure mockId
  findPlaylist _ = pure $ Just mockPlaylist

instance PlaylistTagRepo AppMockT where
  insertPlaylistTag _ _ = pure ()
  findPlaylistTagsByPlaylist _ =
    pure [PlaylistTag { playlistTagId = mockId, playlistTagName = "" }]

mockId :: Id a
mockId = 12345

mockUser :: User
mockUser = User
  { userId        = mockId
  , userUsername  = "mockUser"
  , userPassword  = "mockUser"
  , userCreatedOn = unsafePerformIO $ Time.getCurrentTime
  }

mockPlaylist :: Playlist
mockPlaylist = Playlist
  { playlistId        = mockId
  , playlistName      = "My mock playlist"
  , playlistStyle     = Unordered
  , playlistPrivacy   = Public
  , playlistCreatedOn = unsafePerformIO $ Time.getCurrentTime
  }

mockPlaylistTag :: PublicPlaylistTag
mockPlaylistTag = PublicPlaylistTag { name = "Test" }

mockGetPlaylistResponse :: GetPlaylistResponse
mockGetPlaylistResponse = GetPlaylistResponse
  { id        = playlistId mockPlaylist
  , name      = playlistName mockPlaylist
  , style     = playlistStyle mockPlaylist
  , privacy   = playlistPrivacy mockPlaylist
  , createdOn = playlistCreatedOn mockPlaylist
  , tags      = [mockPlaylistTag]
  }
