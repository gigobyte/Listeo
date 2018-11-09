module Utils.Api exposing (endpoint, isLoading)

import RemoteData exposing (RemoteData(..), WebData)


endpoint : String -> String
endpoint =
    (++) "http://localhost:8081"


isLoading : WebData a -> Bool
isLoading request =
    case request of
        Loading ->
            True

        _ ->
            False
