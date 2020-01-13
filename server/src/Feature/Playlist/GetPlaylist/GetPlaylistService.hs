module Feature.Playlist.GetPlaylist.GetPlaylistService where

import Protolude
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Feature.Playlist.Playlist
import Control.Monad.Trans.Maybe
import Feature.User.User (User(..))
import Infrastructure.Utils.Maybe (liftMaybe)
import Feature.PlaylistTag.PlaylistTag (toPublicPlaylistTag)
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import Feature.Video.VideoRepoClass (VideoRepo(..))
import Infrastructure.Utils.Id (Id)
import Feature.Video.Video (toPublicVideo)
import Feature.Playlist.GetPlaylist.GetPlaylistResult
  (GetPlaylistError(..), GetPlaylistResponse(..))

getPlaylist
  :: (PlaylistRepo m, PlaylistTagRepo m, VideoRepo m)
  => LByteString
  -> Maybe User
  -> m (Either GetPlaylistError GetPlaylistResponse)
getPlaylist rawPlaylistId user =
  maybeToEither PlaylistNotFound
    <$> (runMaybeT $ do
          reqPlaylistId <- liftMaybe $ readPlaylistIdFromParam rawPlaylistId
          playlist      <-
            MaybeT
            $   mfilter (isPlaylistViewable user)
            <$> findPlaylist reqPlaylistId

          playlistTags   <- lift $ findPlaylistTagsByPlaylist reqPlaylistId
          playlistVideos <- lift $ findVideosByPlaylist reqPlaylistId

          return GetPlaylistResponse
            { id          = playlistId playlist
            , name        = playlistName playlist
            , description = playlistDescription playlist
            , style       = playlistStyle playlist
            , privacy     = playlistPrivacy playlist
            , createdOn   = playlistCreatedOn playlist
            , tags        = toPublicPlaylistTag <$> playlistTags
            , videos      = toPublicVideo <$> playlistVideos
            }
        )

readPlaylistIdFromParam :: LByteString -> Maybe (Id Playlist)
readPlaylistIdFromParam = readMaybe . T.unpack . decodeUtf8 . B.toStrict
