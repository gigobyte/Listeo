module Infrastructure.DB
    ( runQuery
    ) where

import           Database.MongoDB (Action, Pipe, access, master)
import           Protolude

runQuery :: Pipe -> Action IO a -> IO a
runQuery pipe =
    access pipe master "listeodb"
