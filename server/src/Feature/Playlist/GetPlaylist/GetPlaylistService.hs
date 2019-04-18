module Feature.Playlist.GetPlaylist.GetPlaylistService where

import Protolude
import qualified Data.ByteString.Lazy as B
import Feature.Playlist.PlaylistRepoClass (PlaylistRepo(..))
import Feature.PlaylistTag.PlaylistTagRepoClass (PlaylistTagRepo(..))
import Feature.Playlist.GetPlaylist.GetPlaylistError
import Feature.Playlist.GetPlaylist.GetPlaylistResponse

getPlaylist
  :: (PlaylistRepo m, PlaylistTagRepo m)
  => LByteString
  -> m (Either GetPlaylistError GetPlaylistResponse)
getPlaylist rawPlaylistId = do
  let playlistId = decodeUtf8 $ B.toStrict rawPlaylistId
  playlist     <- findPlaylist playlistId
  playlistTags <- findPlaylistTagsByPlaylist playlistId
  undefined
