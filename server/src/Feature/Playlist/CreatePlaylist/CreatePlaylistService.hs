module Feature.Playlist.CreatePlaylist.CreatePlaylistService where

import Protolude
import Data.Aeson
import Control.Monad.Except (liftEither)
import Infrastructure.Utils.Id
import Feature.Playlist.Playlist
import Feature.Playlist.CreatePlaylist.CreatePlaylistResult
import Feature.Playlist.PlaylistRepoClass
import Feature.PlaylistTag.PlaylistTagRepoClass
import Feature.User.User
import qualified Data.Text as T

data CreatePlaylist = CreatePlaylist
    { createPlaylistName :: Text
    , createPlaylistDescription :: Text
    , createPlaylistTags :: [Text]
    , createPlaylistPrivacy :: PlaylistPrivacy
    , createPlaylistStyle :: PlaylistStyle
    }

instance FromJSON CreatePlaylist where
  parseJSON = withObject "createPlaylist" $ \o ->
    CreatePlaylist
      <$> o
      .:  "name"
      <*> o
      .:  "description"
      <*> o
      .:  "tags"
      <*> o
      .:  "privacy"
      <*> o
      .:  "style"

createPlaylist
  :: (PlaylistRepo m, PlaylistTagRepo m)
  => LByteString
  -> User
  -> m (Either CreatePlaylistError (Id Playlist))
createPlaylist rawBody user = runExceptT $ do
  body               <- liftEither $ parseBody rawBody
  playlist           <- liftEither $ mkInsertPlaylist body user
  insertedPlaylistId <- lift $ insertPlaylist playlist
  tags <- liftEither $ mkInsertPlaylistTags body insertedPlaylistId

  lift $ forM_ tags insertPlaylistTag

  return insertedPlaylistId

parseBody :: LByteString -> Either CreatePlaylistError CreatePlaylist
parseBody body = maybeToRight InvalidRequest (decode body)

mkInsertPlaylist
  :: CreatePlaylist -> User -> Either CreatePlaylistError InsertPlaylist
mkInsertPlaylist req user =
  maybeToRight ValidationFailed
    $   InsertPlaylist
    <$> (validatePlaylistName $ createPlaylistName req)
    <*> pure (createPlaylistDescription req)
    <*> pure (createPlaylistStyle req)
    <*> pure (createPlaylistPrivacy req)
    <*> pure (userId user)

mkInsertPlaylistTags
  :: CreatePlaylist
  -> Id Playlist
  -> Either CreatePlaylistError [InsertPlaylistTag]
mkInsertPlaylistTags body playlistId =
  sequence $ (mkInsertPlaylistTag playlistId) <$> (createPlaylistTags body)

mkInsertPlaylistTag
  :: Id Playlist -> Text -> Either CreatePlaylistError InsertPlaylistTag
mkInsertPlaylistTag playlistId tagName =
  maybeToRight ValidationFailed
    $   InsertPlaylistTag
    <$> (validatePlaylistTag tagName)
    <*> (pure playlistId)

validatePlaylistName :: Text -> Maybe Text
validatePlaylistName str
  | T.length str == 0 = Nothing
  | T.length str > 99 = Nothing
  | otherwise         = Just (T.strip str)

validatePlaylistTag :: Text -> Maybe Text
validatePlaylistTag str
  | T.length str == 0 = Nothing
  | T.length str > 99 = Nothing
  | otherwise         = Just (T.strip str)
