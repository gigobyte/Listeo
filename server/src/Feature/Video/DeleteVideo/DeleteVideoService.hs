module Feature.Video.DeleteVideo.DeleteVideoService where

import Protolude
import Feature.Video.VideoRepoClass
import Feature.Playlist.PlaylistRepoClass
import Control.Monad.Except
import Feature.Playlist.Playlist
import Infrastructure.Utils.Id
import Feature.Video.DeleteVideo.DeleteVideoResult
import Feature.User.User

deletePlaylistVideo
  :: (VideoRepo m, PlaylistRepo m)
  => LByteString
  -> LByteString
  -> User
  -> m (Either DeleteVideoError ())
deletePlaylistVideo rawPlaylistId rawVideoId user = runExceptT $ do
  reqPlaylistId <- liftEither $ parseId rawPlaylistId
  reqVideoId    <- liftEither $ parseId rawVideoId
  playlist      <-
    ExceptT $ maybeToRight InvalidRequest <$> findPlaylist reqPlaylistId

  when (playlistAuthorId playlist /= userId user)
    $ throwE UnauthorizedToDeleteVideo

  _ <- ExceptT $ maybeToRight VideoNotFound <$> findVideo reqVideoId

  lift $ deleteVideo reqVideoId

parseId :: LByteString -> Either DeleteVideoError (Id a)
parseId playlistId = maybeToRight InvalidRequest (getIdFromParam playlistId)
