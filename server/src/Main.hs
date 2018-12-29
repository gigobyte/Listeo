module Main where

import Protolude hiding (get)
import Flow
import Web.Scotty
import Network.Wai.Middleware.Cors
import qualified Database.MongoDB as DB
import qualified Feature.Login.HTTP as HTTP
import qualified Feature.Register.HTTP as HTTP
import qualified Feature.User.HTTP as HTTP
import qualified Infrastructure.Middleware.Auth as Middleware

server :: DB.Pipe -> ScottyM ()
server pipe = do
  post "/register" <| HTTP.register pipe
  post "/login" <| HTTP.login pipe
  get "/me" <| Middleware.auth HTTP.me pipe

policy :: CorsResourcePolicy
policy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = simpleMethods
  , corsRequestHeaders = [ "Accept"
                         , "Accept-Language"
                         , "Content-Language"
                         , "Content-Type"
                         , "Authorization"
                         ]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }

main :: IO ()
main = do
  pipe <- DB.connect $ DB.host "127.0.0.1"
  scotty 8081 $ do
    middleware (cors $ const (Just policy))
    server pipe
