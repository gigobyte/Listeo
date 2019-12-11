module Feature.Playlist.GetPlaylist.GetPlaylistService where

import Protolude
import qualified Data.ByteString.Lazy as B
import Feature.Playlist.Playlist
import Feature.PlaylistTag.PlaylistTag (toPublicPlaylistTag)
import qualified Feature.Playlist.Playlist as Playlist
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import Feature.Playlist.GetPlaylist.GetPlaylistResult
  (GetPlaylistError(..), GetPlaylistResponse(..))

getPlaylist
  :: (PlaylistRepo m, PlaylistTagRepo m)
  => LByteString
  -> m (Either GetPlaylistError GetPlaylistResponse)
getPlaylist rawPlaylistId = do
  let playlistId = decodeUtf8 $ B.toStrict rawPlaylistId
  maybePlaylist <- findPlaylist playlistId

  case maybePlaylist of
    Just playlist -> do
      playlistTags <- findPlaylistTagsByPlaylist playlistId

      return $ Right GetPlaylistResponse
        { id        = Playlist.playlistId playlist
        , name      = playlistName playlist
        , style     = playlistStyle playlist
        , privacy   = playlistPrivacy playlist
        , createdOn = playlistCreatedOn playlist
        , tags      = toPublicPlaylistTag <$> playlistTags
        }

    Nothing -> return $ Left PlaylistNotFound
