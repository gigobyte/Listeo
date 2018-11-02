module Main where

import           Database.MongoDB
import qualified Feature.User.HTTP           as UserHTTP
import           Network.Wai.Middleware.Cors
import           Protolude                   hiding (get)
import           Web.Scotty

server :: Pipe -> ScottyM ()
server pipe = do
    post "/register" $ UserHTTP.register pipe

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
