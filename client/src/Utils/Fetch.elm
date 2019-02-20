module Utils.Fetch exposing (get, getWithAuth, post, postWithAuth)

import Http exposing (emptyBody, header)


get : { url : String, expect : Http.Expect msg } -> Cmd msg
get options =
    Http.get options


getWithAuth : { url : String, token : String, expect : Http.Expect msg } -> Cmd msg
getWithAuth options =
    Http.request
        { method = "GET"
        , headers = [ header "Authorization" ("Bearer " ++ options.token) ]
        , url = options.url
        , expect = options.expect
        , body = emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


post : { url : String, expect : Http.Expect msg, body : Http.Body } -> Cmd msg
post options =
    Http.post options


postWithAuth : { url : String, token : String, expect : Http.Expect msg, body : Http.Body } -> Cmd msg
postWithAuth options =
    Http.request
        { method = "POST"
        , headers = [ header "Authorization" ("Bearer " ++ options.token) ]
        , url = options.url
        , expect = options.expect
        , body = options.body
        , timeout = Nothing
        , tracker = Nothing
        }
