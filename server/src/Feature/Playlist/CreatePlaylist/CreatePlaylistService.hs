module Feature.Playlist.CreatePlaylist.CreatePlaylistService where

import Protolude
import Data.Aeson (decode)
import Control.Monad.Except (liftEither)
import Infrastructure.Utils.Id (Id)
import Feature.Playlist.CreatePlaylist.CreatePlaylistBody (CreatePlaylistBody)
import Feature.Playlist.CreatePlaylist.CreatePlaylistResult (CreatePlaylistError(..))
import qualified Feature.Playlist.CreatePlaylist.CreatePlaylistBody as CreatePlaylistBody
import Feature.Playlist.Playlist (Playlist)
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..), InsertPlaylist(..))
import Feature.PlaylistTag.PlaylistTagRepoClass
  (PlaylistTagRepo(..), InsertPlaylistTag(..))
import qualified Data.Text as T

createPlaylist
  :: (PlaylistRepo m, PlaylistTagRepo m)
  => LByteString
  -> m (Either CreatePlaylistError (Id Playlist))
createPlaylist rawBody = runExceptT $ do
  body               <- liftEither $ parseBody rawBody
  playlist           <- liftEither $ mkInsertPlaylist body
  insertedPlaylistId <- lift $ insertPlaylist playlist
  tags               <-
    liftEither
    $   sequence
    $   mkInsertPlaylistTag
    <$> (CreatePlaylistBody.tags body)

  lift $ forM_ tags (insertPlaylistTag insertedPlaylistId)

  return insertedPlaylistId

parseBody :: LByteString -> Either CreatePlaylistError CreatePlaylistBody
parseBody body = maybeToRight InvalidRequest (decode body)

mkInsertPlaylist
  :: CreatePlaylistBody -> Either CreatePlaylistError InsertPlaylist
mkInsertPlaylist req =
  maybeToRight ValidationFailed
    $   InsertPlaylist
    <$> (validatePlaylistName $ CreatePlaylistBody.name req)
    <*> pure (CreatePlaylistBody.style req)
    <*> pure (CreatePlaylistBody.privacy req)

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
