module Feature.Playlist.PlaylistRepo
  ( insertPlaylist
  , findPlaylist
  )
where

import Protolude
import Data.Maybe (fromJust)
import Database.MongoDB (findOne, insert, select, cast', (=:))
import Infrastructure.DB (MonadDB, runQuery, withConn)
import Infrastructure.Utils.Id (Id)
import Feature.Playlist.Playlist (Playlist)
import Feature.Playlist.PlaylistRepoClass (InsertPlaylist(..))
import qualified Feature.Playlist.Playlist as Playlist

insertPlaylist :: (MonadDB m) => InsertPlaylist -> m (Id Playlist)
insertPlaylist playlist = withConn $ \conn -> do
  playlistId <- runQuery conn (insert "playlist" playlistBson)
  return $ fromJust $ cast' playlistId
  where
    playlistBson =
      [ "name" =: insertPlaylistName playlist
      , "style" =: (fromEnum $ insertPlaylistStyle playlist)
      , "privacy" =: (fromEnum $ insertPlaylistPrivacy playlist)
      ]

findPlaylist :: (MonadDB m) => Text -> m (Maybe Playlist)
findPlaylist playlistId = withConn $ \conn -> do
  maybePlaylist <- runQuery conn
    $ findOne (select ["_id" =: playlistId] "playlist")
  return $ Playlist.fromBson =<< maybePlaylist
