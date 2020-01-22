module Feature.Playlist.GetPlaylist.GetPlaylistService where

import Protolude
import Feature.Playlist.Playlist
import Control.Monad.Except
import Feature.User.User
import Feature.PlaylistTag.PlaylistTag
import Feature.Playlist.PlaylistRepoClass
import Feature.PlaylistTag.PlaylistTagRepoClass
import Feature.Video.VideoRepoClass
import Infrastructure.Utils.Id
import Feature.Video.Video
import Feature.Playlist.GetPlaylist.GetPlaylistResult

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
