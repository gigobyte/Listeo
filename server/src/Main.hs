module Main where

import           Database.MongoDB
import           Network.Wai.Middleware.Cors
import           Protolude                   hiding (get)
import           Routes.Auth.Register        (register)
import           Web.Scotty

server :: Pipe -> ScottyM ()
server pipe = do
    post "/auth/register" $ register pipe

main :: IO ()
main = do
    pipe <- connect $ host "127.0.0.1"
    scotty 8081 $ do
        middleware simpleCors
        server pipe
