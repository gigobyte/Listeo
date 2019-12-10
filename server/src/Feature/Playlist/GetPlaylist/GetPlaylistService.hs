module Feature.Playlist.GetPlaylist.GetPlaylistService where

import Protolude
import qualified Data.ByteString.Lazy as B
import qualified Feature.Playlist.Playlist as Playlist
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import Feature.Playlist.GetPlaylist.GetPlaylistResult (GetPlaylistError(..), GetPlaylistResponse(..))

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
        { id        = Playlist.id playlist
        , name      = Playlist.name playlist
        , style     = Playlist.style playlist
        , privacy   = Playlist.privacy playlist
        , createdOn = Playlist.createdOn playlist
        , tags      = playlistTags
        }

    Nothing -> return $ Left PlaylistNotFound
