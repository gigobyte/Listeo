module Pages.Login.Api exposing
    ( LoginRequest
    , LoginResponse(..)
    , login
    , loginErrorToString
    )

import Http exposing (expectJson)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData as RemoteData exposing (WebData)
import Utils.Fetch as Fetch exposing (ApiRoot)


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


login : ApiRoot -> LoginRequest -> Cmd (WebData LoginResponse)
login apiRoot model =
    Fetch.post
        { url = Fetch.login apiRoot
        , expect =
            expectJson RemoteData.fromResult
                (Decode.oneOf
                    [ errorResponseDecoder
                    , successResponseDecoder
                    ]
                )
        , body = model |> loginRequestEncoder |> Http.jsonBody
        }


loginRequestEncoder : LoginRequest -> Encode.Value
loginRequestEncoder req =
    Encode.object
        [ ( "username", Encode.string req.username )
        , ( "password", Encode.string req.password )
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


loginErrorToString : LoginResponseError -> String
loginErrorToString err =
    case err of
        UserNotFound ->
            "User not found"

        ServerError ->
            "Something went wrong"

        ValidationFailed ->
            ""
