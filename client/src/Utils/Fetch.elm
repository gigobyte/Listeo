module Utils.Fetch exposing
    ( ApiRoot(..)
    , Endpoint
    , Token(..)
    , createPlaylist
    , currentUser
    , emptyToken
    , get
    , getWithAuth
    , login
    , post
    , postWithAuth
    , register
    )

import Http exposing (emptyBody, header)



-- REQUEST


get : { url : Endpoint, expect : Http.Expect msg } -> Cmd msg
get options =
    Http.get
        { url = unwrapEndpoint options.url
        , expect = options.expect
        }


getWithAuth : { url : Endpoint, token : Token, expect : Http.Expect msg } -> Cmd msg
getWithAuth options =
    Http.request
        { method = "GET"
        , headers = [ header "Authorization" ("Bearer " ++ unwrapToken options.token) ]
        , url = unwrapEndpoint options.url
        , expect = options.expect
        , body = emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


post : { url : Endpoint, expect : Http.Expect msg, body : Http.Body } -> Cmd msg
post options =
    Http.post
        { url = unwrapEndpoint options.url
        , expect = options.expect
        , body = options.body
        }


postWithAuth : { url : Endpoint, token : Token, expect : Http.Expect msg, body : Http.Body } -> Cmd msg
postWithAuth options =
    Http.request
        { method = "POST"
        , headers = [ header "Authorization" ("Bearer " ++ unwrapToken options.token) ]
        , url = unwrapEndpoint options.url
        , expect = options.expect
        , body = options.body
        , timeout = Nothing
        , tracker = Nothing
        }



-- TYPES


type Endpoint
    = Endpoint String ApiRoot


unwrapEndpoint : Endpoint -> String
unwrapEndpoint (Endpoint url apiRoot) =
    unwrapApiRoot apiRoot ++ url


type ApiRoot
    = ApiRoot String


unwrapApiRoot : ApiRoot -> String
unwrapApiRoot (ApiRoot str) =
    str


type Token
    = Token (Maybe String)


unwrapToken : Token -> String
unwrapToken (Token maybeStr) =
    case maybeStr of
        Just str ->
            str

        Nothing ->
            ""


emptyToken : Token
emptyToken =
    Token Nothing



-- ENDPOINTS


login : ApiRoot -> Endpoint
login =
    Endpoint "/login"


register : ApiRoot -> Endpoint
register =
    Endpoint "/register"


currentUser : ApiRoot -> Endpoint
currentUser =
    Endpoint "/me"


createPlaylist : ApiRoot -> Endpoint
createPlaylist =
    Endpoint "/playlist"
