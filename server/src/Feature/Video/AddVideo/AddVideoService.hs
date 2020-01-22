module Feature.Video.AddVideo.AddVideoService where

import Protolude
import Data.Aeson
import Feature.Video.VideoRepoClass
import Feature.Playlist.Playlist
import Feature.Playlist.PlaylistRepoClass
import Feature.Video.AddVideo.AddVideoResult
import Control.Monad.Except
import Feature.User.User
import Infrastructure.Utils.Id
import Feature.Video.Video

data AddVideo = AddVideo
  { addVideoUrl :: Text
  , addVideoNote :: Text
  , addVideoTags :: [Text]
  }

instance FromJSON AddVideo where
  parseJSON = withObject "addVideo"
    $ \o -> AddVideo <$> o .: "url" <*> o .: "note" <*> o .: "tags"

addVideoToPlaylist
  :: (VideoRepo m, PlaylistRepo m)
  => LByteString
  -> LByteString
  -> User
  -> m (Either AddVideoError (Id Video))
addVideoToPlaylist rawPlaylistId rawBody user = runExceptT $ do
  body          <- liftEither $ parseBody rawBody
  reqPlaylistId <- liftEither $ parsePlaylistId rawPlaylistId
  playlist      <-
    ExceptT $ maybeToRight InvalidRequest <$> findPlaylist reqPlaylistId

  when (playlistAuthorId playlist /= userId user)
    $ throwE UnauthorizedToEditPlaylist

  video           <- liftEither $ mkInsertVideo body reqPlaylistId
  insertedVideoId <- lift $ insertVideo video
  tags            <- liftEither $ mkInsertVideoTags body insertedVideoId

  lift $ forM_ tags insertVideoTag

  return insertedVideoId

parseBody :: LByteString -> Either AddVideoError AddVideo
parseBody body = maybeToRight InvalidRequest (decode body)

parsePlaylistId :: LByteString -> Either AddVideoError (Id Playlist)
parsePlaylistId playlistId =
  maybeToRight InvalidRequest (getIdFromParam playlistId)

mkInsertVideoTags
  :: AddVideo -> Id Video -> Either AddVideoError [InsertVideoTag]
mkInsertVideoTags req videoId =
  sequence $ (mkInsertPlaylistTag videoId) <$> (addVideoTags req)

mkInsertPlaylistTag :: Id Video -> Text -> Either AddVideoError InsertVideoTag
mkInsertPlaylistTag videoId tagName =
  maybeToRight ValidationFailed
    $   InsertVideoTag
    <$> (pure tagName)
    <*> (pure videoId)

mkInsertVideo :: AddVideo -> Id Playlist -> Either AddVideoError InsertVideo
mkInsertVideo req playlistId =
  maybeToRight ValidationFailed
    $   InsertVideo
    <$> pure (addVideoUrl req)
    <*> pure (addVideoNote req)
    <*> pure playlistId
