module Infrastructure.Middleware.Cors where

import Protolude
import Web.Scotty.Trans
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Method

policy :: CorsResourcePolicy
policy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = methodDelete : simpleMethods
  , corsRequestHeaders =
    [ "Accept"
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

cors :: ScottyT e m ()
cors = middleware (Network.Wai.Middleware.Cors.cors $ const (Just policy))
