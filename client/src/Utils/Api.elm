module Utils.Api exposing (endpoint)

import RemoteData exposing (RemoteData(..), WebData)


endpoint : String -> String
endpoint =
    (++) "http://localhost:8081"
