module Feature.Playlist.GetPlaylist.GetPlaylistService where

import Protolude
import Feature.Playlist.Playlist
import Control.Monad.Except (liftEither)
import Feature.User.User (User(..))
import Feature.PlaylistTag.PlaylistTag (toPublicPlaylistTag)
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import Feature.Video.VideoRepoClass (VideoRepo(..))
import Infrastructure.Utils.Id (getIdFromParam)
import Feature.Video.Video (toPublicVideo)
import Feature.Playlist.GetPlaylist.GetPlaylistResult
  (GetPlaylistError(..), GetPlaylistResponse(..))

getPlaylist
  :: (PlaylistRepo m, PlaylistTagRepo m, VideoRepo m)
  => LByteString
  -> Maybe User
  -> m (Either GetPlaylistError GetPlaylistResponse)
getPlaylist rawPlaylistId user = runExceptT $ do
  reqPlaylistId <- liftEither $ maybeToRight InvalidRequest $ getIdFromParam
    rawPlaylistId
  playlist <-
    ExceptT $ maybeToRight PlaylistNotFound <$> findPlaylist reqPlaylistId

  when (not $ isPlaylistViewable user playlist) $ throwE PlaylistIsPrivate

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
