module Feature.Playlist.PlaylistRepo
  ( insertPlaylist
  )
where

import Protolude
import Data.Maybe (fromJust)
import Database.MongoDB (insert, cast')
import Infrastructure.DB (MonadDB, runQuery, withConn)
import Infrastructure.Utils.Id (Id)
import Feature.Playlist.Playlist (Playlist)
import Feature.Playlist.PlaylistDTO (PlaylistDTO)
import qualified Feature.Playlist.PlaylistDTO as PlaylistDTO

insertPlaylist :: (MonadDB m) => PlaylistDTO -> m (Id Playlist)
insertPlaylist playlist = withConn $ \conn -> do
  playlistId <- runQuery conn (insert "playlist" $ PlaylistDTO.toBson playlist)
  return $ fromJust $ cast' playlistId
