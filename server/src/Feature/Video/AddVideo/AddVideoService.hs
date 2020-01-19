module Feature.Video.AddVideo.AddVideoService where

import Protolude
import Data.Aeson
import Feature.Video.VideoRepoClass
import Feature.Playlist.Playlist
import Feature.Playlist.PlaylistRepoClass
import Feature.Video.AddVideo.AddVideoResult
import Control.Monad.Except (liftEither)
import Feature.User.User (User(..))
import Infrastructure.Utils.Id (Id, getIdFromParam)
import Feature.Video.Video (Video)

data AddVideo = AddVideo
  { addVideoUrl :: Text
  , addVideoNote :: Text
  }

instance FromJSON AddVideo where
  parseJSON =
    withObject "addVideo" $ \o -> AddVideo <$> o .: "url" <*> o .: "note"

addVideoToPlaylist
  :: (VideoRepo m, PlaylistRepo m)
  => LByteString
  -> LByteString
  -> User
  -> m (Either AddVideoError (Id Video))
addVideoToPlaylist rawPlaylistId rawBody user = runExceptT $ do
  body          <- liftEither $ parseBody rawBody
  reqPlaylistId <- liftEither $ maybeToRight InvalidRequest $ getIdFromParam
    rawPlaylistId
  playlist <-
    ExceptT $ maybeToRight InvalidRequest <$> findPlaylist reqPlaylistId

  when (playlistAuthorId playlist /= userId user)
    $ throwE UnauthorizedToEditPlaylist

  return reqPlaylistId

parseBody :: LByteString -> Either AddVideoError AddVideo
parseBody body = maybeToRight InvalidRequest (decode body)
