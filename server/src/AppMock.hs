module AppMock where

import Protolude hiding (get)
import Env
import System.IO.Unsafe (unsafePerformIO)
import Infrastructure.MonadTime
import Infrastructure.MonadCrypto
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
import Feature.Video.VideoRepoClass (VideoRepo(..))
import Feature.Video.VideoServiceClass (VideoService(..))
import Feature.PlaylistTag.PlaylistTag
import qualified Infrastructure.Utils.Crypto as Crypto
import qualified Data.Time.Clock as Time

newtype AppMockT a = AppMockT
    { unAppMockT :: ReaderT Env IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

instance MonadTime AppMockT where
  getCurrentTime = liftIO Time.getCurrentTime

instance MonadCrypto AppMockT where
  hash = Crypto.hash

instance UserRepo AppMockT where
  insertUser _ = pure ()
  deleteUser _ = pure ()
  findUserByUsername _ = pure $ Just mockUser
  findUserByEmail _ = pure $ Just mockUser

instance AuthService AppMockT where
  requireUser  = pure mockUser
  optionalUser = pure Nothing
  deleteCurrentUser _ = pure ()

instance RegisterService AppMockT where
  register _ = pure $ Right ""

instance LoginService AppMockT where
  login _ = pure $ Right "some jwt"

instance PlaylistService AppMockT where
  createPlaylist _ _ = pure $ Right 12345
  getPlaylist _ _ = pure $ Right mockGetPlaylistResponse

instance PlaylistRepo AppMockT where
  insertPlaylist _ = pure 12345
  findPlaylist _ = pure $ Just mockPlaylist

instance PlaylistTagRepo AppMockT where
  insertPlaylistTag _ = pure ()
  findPlaylistTagsByPlaylist _ =
    pure [PlaylistTag { playlistTagId = 12345, playlistTagName = "" }]

instance VideoRepo AppMockT where
  findVideosByPlaylist _ = pure []
  findTagsByVideo _ = pure []
  insertVideo _ = pure 12345
  insertVideoTag _ = pure ()
  getVideoMetadata _ = pure Nothing

instance VideoService AppMockT where
  addVideoToPlaylist _ _ _ = pure $ Right 12345

mockUser :: User
mockUser = User
  { userId        = 12345
  , userUsername  = "mockUser"
  , userEmail     = "mock@user"
  , userPassword  = "mockUser"
  , userCreatedOn = unsafePerformIO $ Time.getCurrentTime
  }

mockPlaylist :: Playlist
mockPlaylist = Playlist
  { playlistId          = 12345
  , playlistAuthorId    = 123
  , playlistName        = "My mock playlist"
  , playlistDescription = ""
  , playlistStyle       = Unordered
  , playlistPrivacy     = Public
  , playlistCreatedOn   = unsafePerformIO $ Time.getCurrentTime
  }

mockPlaylistTag :: PublicPlaylistTag
mockPlaylistTag = PublicPlaylistTag { name = "Test" }

mockGetPlaylistResponse :: GetPlaylistResponse
mockGetPlaylistResponse = GetPlaylistResponse
  { id          = playlistId mockPlaylist
  , name        = playlistName mockPlaylist
  , description = playlistDescription mockPlaylist
  , style       = playlistStyle mockPlaylist
  , privacy     = playlistPrivacy mockPlaylist
  , createdOn   = playlistCreatedOn mockPlaylist
  , tags        = [mockPlaylistTag]
  , videos      = []
  }
