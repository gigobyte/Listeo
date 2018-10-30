module Main where

import           Database.MongoDB
import qualified Feature.User.HTTP           as UserHTTP
import           Network.Wai.Middleware.Cors
import           Protolude                   hiding (get)
import           Web.Scotty

server :: Pipe -> ScottyM ()
server pipe = do
    post "/auth/register" $ UserHTTP.register pipe

main :: IO ()
main = do
    pipe <- connect $ host "127.0.0.1"
    scotty 8081 $ do
        middleware simpleCors
        server pipe
