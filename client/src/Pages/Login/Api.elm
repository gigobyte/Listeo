module Pages.Login.Api exposing
    ( LoginRequest
    , LoginResponse(..)
    , LoginResponseError(..)
    , login
    )

import Http exposing (expectJson)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData as RemoteData exposing (WebData)
import Utils.Fetch as Fetch exposing (ApiRoot)


login : ApiRoot -> LoginRequest -> Cmd (WebData LoginResponse)
login apiRoot model =
    Fetch.post
        { url = Fetch.login apiRoot
        , expect =
            expectJson RemoteData.fromResult loginResponseDecoder
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
    = ValidationFailed
    | UserNotFound
    | ServerError


type LoginResponse
    = ErrorResponse { errorDescription : LoginResponseError }
    | SuccessResponse { jwt : String }


loginResponseDecoder : Decoder LoginResponse
loginResponseDecoder =
    Decode.oneOf
        [ -- Success
          Decode.succeed (\x -> { jwt = x })
            |> required "jwt" Decode.string
            |> Decode.andThen (Decode.succeed << SuccessResponse)

        -- Error
        , Decode.succeed (\x -> { errorDescription = x })
            |> required "errorDescription"
                (Decode.string
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
                )
            |> Decode.andThen (Decode.succeed << ErrorResponse)
        ]
