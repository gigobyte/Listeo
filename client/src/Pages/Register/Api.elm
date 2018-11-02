module Pages.Register.Api exposing
    ( RegisterRequest
    , makeRegisterRequestModel
    , register
    )

import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Msg exposing (Msg(..))
import Pages.Register.Model exposing (Model, RegisterError(..), RegisterResponse)
import Pages.Register.Validation exposing (RegisterValidationError, registerValidator)
import RemoteData as RemoteData
import Validate exposing (fromValid, validate)


type alias RegisterRequest =
    { username : String
    , password : String
    }


registerRequestEncoder : RegisterRequest -> Encode.Value
registerRequestEncoder req =
    Encode.object
        [ ( "username", Encode.string req.username )
        , ( "password", Encode.string req.password )
        ]


makeRegisterRequestModel : Model -> Maybe RegisterRequest
makeRegisterRequestModel model =
    validate registerValidator model
        |> Result.map fromValid
        |> Result.map
            (\validatedModel ->
                { username = validatedModel.username
                , password = validatedModel.password
                }
            )
        |> Result.toMaybe


register : RegisterRequest -> Cmd Msg
register model =
    Http.post "http://localhost:8081/register" (Http.jsonBody (registerRequestEncoder model)) registerResponseDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Register


registerResponseDecoder : Decoder RegisterResponse
registerResponseDecoder =
    Decode.succeed RegisterResponse
        |> required "errorDescription" registerErrorDecoder


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
