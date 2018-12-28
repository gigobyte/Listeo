module Infrastructure.DB
  ( runQuery
  )
where

import Protolude
import Database.MongoDB (Action, Pipe, access, master)

runQuery :: Pipe -> Action IO a -> IO a
runQuery pipe = access pipe master "listeodb"
