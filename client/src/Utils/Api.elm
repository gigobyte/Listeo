module Utils.Api exposing (endpoint)


endpoint : String -> String
endpoint =
    (++) "http://localhost:8081"
