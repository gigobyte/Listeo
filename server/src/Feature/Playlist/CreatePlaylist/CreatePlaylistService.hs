module Feature.Playlist.CreatePlaylist.CreatePlaylistService where

import Protolude
import Data.Aeson (decode)
import Control.Monad.Except (liftEither)
import Infrastructure.AppError
import Feature.Playlist.CreatePlaylist.CreatePlaylistBody (CreatePlaylistBody)
import qualified Feature.Playlist.CreatePlaylist.CreatePlaylistBody as CreatePlaylistBody
import Feature.Playlist.PlaylistDTO (PlaylistDTO(..))
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagDTO (PlaylistTagDTO(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import qualified Data.Text as T

createPlaylist
  :: (PlaylistRepo m, PlaylistTagRepo m)
  => LByteString
  -> m (Either (AppError ()) ())
createPlaylist rawBody = runExceptT $ do
  body               <- liftEither $ parseBody rawBody
  playlist           <- liftEither $ mkPlaylistDTO body
  insertedPlaylistId <- lift $ insertPlaylist playlist
  tags               <-
    liftEither $ sequence $ mkPlaylistTagDTO <$> (CreatePlaylistBody.tags body)

  lift $ forM_ tags (insertPlaylistTag insertedPlaylistId)

parseBody :: LByteString -> Either (AppError ()) CreatePlaylistBody
parseBody body = maybeToRight InvalidRequest (decode body)

mkPlaylistDTO :: CreatePlaylistBody -> Either (AppError ()) PlaylistDTO
mkPlaylistDTO req =
  maybeToRight ValidationFailed
    $   PlaylistDTO
    <$> (validatePlaylistName $ CreatePlaylistBody.name req)
    <*> pure (CreatePlaylistBody.style req)
    <*> pure (CreatePlaylistBody.privacy req)

mkPlaylistTagDTO :: Text -> Either (AppError ()) PlaylistTagDTO
mkPlaylistTagDTO tagName =
  maybeToRight ValidationFailed
    $   PlaylistTagDTO
    <$> (validatePlaylistTag tagName)

validatePlaylistName :: Text -> Maybe Text
validatePlaylistName str
  | T.length str < 1 = Nothing
  | otherwise        = Just (T.strip str)

validatePlaylistTag :: Text -> Maybe Text
validatePlaylistTag str
  | T.length str < 1 = Nothing
  | otherwise        = Just (T.strip str)
