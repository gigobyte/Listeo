module Feature.Playlist.CreatePlaylist.HTTP
  ( createPlaylist
  )
where

import Protolude
import qualified Database.MongoDB as DB
import qualified Web.Scotty as Scotty

createPlaylist :: DB.Pipe -> Scotty.ActionM ()
createPlaylist pipe = do
  return ()
