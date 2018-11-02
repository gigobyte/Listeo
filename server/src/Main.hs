module Main where

import           Database.MongoDB
import qualified Feature.Login.HTTP          as HTTP
import qualified Feature.Register.HTTP       as HTTP
import           Network.Wai.Middleware.Cors
import           Protolude                   hiding (get)
import           Web.Scotty

server :: Pipe -> ScottyM ()
server pipe = do
    post "/register" $ HTTP.register pipe
    post "/login" $ HTTP.login pipe

policy :: CorsResourcePolicy
policy =
    CorsResourcePolicy
        { corsOrigins = Nothing
        , corsMethods = simpleMethods
        , corsRequestHeaders = simpleHeaders
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

main :: IO ()
main = do
    pipe <- connect $ host "127.0.0.1"
    scotty 8081 $ do
        middleware (cors $ const (Just policy))
        server pipe
