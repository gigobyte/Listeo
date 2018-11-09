module Pages.Register.Api exposing
    ( RegisterRequest
    , RegisterResponse
    , register
    , registerErrorToString
    )

import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData as RemoteData exposing (WebData)
import Utils.Api exposing (endpoint)


type alias RegisterRequest =
    { username : String
    , password : String
    }


type RegisterResponseError
    = ValidationFailed
    | UserAlreadyExists
    | ServerError


type alias RegisterResponse =
    { errorDescription : Maybe RegisterResponseError
    }


register : RegisterRequest -> Cmd (WebData RegisterResponse)
register model =
    Http.post (endpoint "/register") (model |> registerRequestEncoder |> Http.jsonBody) registerResponseDecoder
        |> RemoteData.sendRequest


registerRequestEncoder : RegisterRequest -> Encode.Value
registerRequestEncoder req =
    Encode.object
        [ ( "username", Encode.string req.username )
        , ( "password", Encode.string req.password )
        ]


registerResponseDecoder : Decoder RegisterResponse
registerResponseDecoder =
    Decode.succeed RegisterResponse
        |> required "errorDescription" registerErrorDecoder


registerErrorDecoder : Decoder (Maybe RegisterResponseError)
registerErrorDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "ValidationFailed" ->
                        Decode.succeed <| Just ValidationFailed

                    "UserAlreadyExists" ->
                        Decode.succeed <| Just UserAlreadyExists

                    "ServerError" ->
                        Decode.succeed <| Just ServerError

                    _ ->
                        Decode.succeed Nothing
            )


registerErrorToString : RegisterResponseError -> String
registerErrorToString err =
    case err of
        UserAlreadyExists ->
            "User already exists"

        ServerError ->
            "Something went wrong"

        ValidationFailed ->
            ""
