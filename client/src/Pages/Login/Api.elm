module Pages.Login.Api exposing
    ( LoginRequest
    , LoginResponse
    , login
    )

import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData as RemoteData exposing (WebData)
import Utils.Api exposing (endpoint)
import Validate exposing (fromValid, validate)


type alias LoginRequest =
    { username : String
    , password : String
    }


type LoginResponseError
    = ValidationFailed
    | UserNotFound
    | ServerError


type LoginResponse
    = ErrorResponse { errorDescription : LoginResponseError }
    | SuccessResponse { jwt : String }


login : LoginRequest -> Cmd (WebData LoginResponse)
login model =
    Http.post (endpoint "/login") (model |> loginRequestEncoder |> Http.jsonBody) loginResponseDecoder
        |> RemoteData.sendRequest


loginRequestEncoder : LoginRequest -> Encode.Value
loginRequestEncoder req =
    Encode.object
        [ ( "username", Encode.string req.username )
        , ( "password", Encode.string req.password )
        ]


loginResponseDecoder : Decoder LoginResponse
loginResponseDecoder =
    Decode.oneOf
        [ errorResponseDecoder
        , successResponseDecoder
        ]


successResponseDecoder : Decoder LoginResponse
successResponseDecoder =
    Decode.succeed (\x -> { jwt = x })
        |> required "jwt" Decode.string
        |> Decode.andThen (Decode.succeed << SuccessResponse)


errorResponseDecoder : Decoder LoginResponse
errorResponseDecoder =
    Decode.succeed (\x -> { errorDescription = x })
        |> required "errorDescription" loginErrorDecoder
        |> Decode.andThen (Decode.succeed << ErrorResponse)


loginErrorDecoder : Decoder LoginResponseError
loginErrorDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "ValidationFailed" ->
                        Decode.succeed ValidationFailed

                    "UserNotFound" ->
                        Decode.succeed UserNotFound

                    "ServerError" ->
                        Decode.succeed ServerError

                    _ ->
                        Decode.succeed ServerError
            )
