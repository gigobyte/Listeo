module Feature.Playlist.CreatePlaylist.CreatePlaylistService where

import Protolude
import Data.Aeson
import Control.Monad.Except (liftEither)
import Infrastructure.Utils.Id (Id)
import Feature.Playlist.Playlist (Playlist, PlaylistStyle, PlaylistPrivacy)
import Feature.Playlist.CreatePlaylist.CreatePlaylistResult
  (CreatePlaylistError(..))
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..), InsertPlaylist(..))
import Feature.PlaylistTag.PlaylistTagRepoClass
  (PlaylistTagRepo(..), InsertPlaylistTag(..))
import qualified Data.Text as T

data CreatePlaylist = CreatePlaylist
    { createPlaylistName :: Text
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
      .:  "tags"
      <*> o
      .:  "privacy"
      <*> o
      .:  "style"

createPlaylist
  :: (PlaylistRepo m, PlaylistTagRepo m)
  => LByteString
  -> m (Either CreatePlaylistError (Id Playlist))
createPlaylist rawBody = runExceptT $ do
  body               <- liftEither $ parseBody rawBody
  playlist           <- liftEither $ mkInsertPlaylist body
  insertedPlaylistId <- lift $ insertPlaylist playlist
  tags               <- liftEither $ mkInsertPlaylistTags body

  lift $ forM_ tags (insertPlaylistTag insertedPlaylistId)

  return insertedPlaylistId

parseBody :: LByteString -> Either CreatePlaylistError CreatePlaylist
parseBody body = maybeToRight InvalidRequest (decode body)

mkInsertPlaylist :: CreatePlaylist -> Either CreatePlaylistError InsertPlaylist
mkInsertPlaylist req =
  maybeToRight ValidationFailed
    $   InsertPlaylist
    <$> (validatePlaylistName $ createPlaylistName req)
    <*> pure (createPlaylistStyle req)
    <*> pure (createPlaylistPrivacy req)

mkInsertPlaylistTags
  :: CreatePlaylist -> Either CreatePlaylistError [InsertPlaylistTag]
mkInsertPlaylistTags body =
  sequence $ mkInsertPlaylistTag <$> (createPlaylistTags body)

mkInsertPlaylistTag :: Text -> Either CreatePlaylistError InsertPlaylistTag
mkInsertPlaylistTag tagName =
  maybeToRight ValidationFailed
    $   InsertPlaylistTag
    <$> (validatePlaylistTag tagName)

validatePlaylistName :: Text -> Maybe Text
validatePlaylistName str
  | T.length str < 1 = Nothing
  | otherwise        = Just (T.strip str)

validatePlaylistTag :: Text -> Maybe Text
validatePlaylistTag str
  | T.length str < 1 = Nothing
  | otherwise        = Just (T.strip str)
