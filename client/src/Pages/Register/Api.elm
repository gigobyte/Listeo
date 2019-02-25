module Pages.Register.Api exposing
    ( RegisterRequest
    , RegisterResponse
    , register
    , registerErrorToString
    )

import Http exposing (expectJson)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import RemoteData as RemoteData exposing (WebData)
import Utils.Fetch as Fetch exposing (ApiRoot)


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


register : ApiRoot -> RegisterRequest -> Cmd (WebData RegisterResponse)
register apiRoot model =
    Fetch.post
        { url = Fetch.register apiRoot
        , body = model |> registerRequestEncoder |> Http.jsonBody
        , expect = expectJson RemoteData.fromResult registerResponseDecoder
        }


registerRequestEncoder : RegisterRequest -> Encode.Value
registerRequestEncoder req =
    Encode.object
        [ ( "username", Encode.string req.username )
        , ( "password", Encode.string req.password )
        ]


registerResponseDecoder : Decoder RegisterResponse
registerResponseDecoder =
    Decode.succeed RegisterResponse
        |> optional "errorDescription" registerErrorDecoder Nothing


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
