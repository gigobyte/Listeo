module Pages.Login.Api exposing
    ( LoginRequest
    , LoginResponse
    , LoginResponseError(..)
    , login
    )

import Enum exposing (Enum)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Utils.ErrorResponse as ErrorResponse exposing (HttpError, ResponseData, expectJsonWithError)
import Utils.Fetch as Fetch exposing (ApiRoot)


login : ApiRoot -> LoginRequest -> Cmd (ResponseData LoginResponseError LoginResponse)
login apiRoot model =
    Fetch.post
        { url = Fetch.login apiRoot
        , expect =
            expectJsonWithError loginResponseErrorEnum.decoder loginResponseDecoder
        , body = model |> loginRequestEncoder |> Http.jsonBody
        }


type alias LoginRequest =
    { username : String
    , password : String
    }


loginRequestEncoder : LoginRequest -> Encode.Value
loginRequestEncoder req =
    Encode.object
        [ ( "username", Encode.string req.username )
        , ( "password", Encode.string req.password )
        ]


type LoginResponseError
    = UserNotFound
    | InvalidRequest
    | ServerError


loginResponseErrorEnum : Enum LoginResponseError
loginResponseErrorEnum =
    Enum.create
        [ ( "UserNotFound", UserNotFound )
        , ( "InvalidRequest", InvalidRequest )
        , ( "ServerError", ServerError )
        ]


type alias LoginResponse =
    { jwt : String }


loginResponseDecoder : Decoder LoginResponse
loginResponseDecoder =
    Decode.succeed (\x -> { jwt = x })
        |> required "jwt" Decode.string
