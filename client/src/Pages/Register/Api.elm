module Pages.Register.Api exposing
    ( RegisterRequest
    , RegisterResponseError(..)
    , register
    )

import Enum exposing (Enum)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Utils.ErrorResponse exposing (ResponseData, expectJsonWithError)
import Utils.Fetch as Fetch exposing (ApiRoot)


register : ApiRoot -> RegisterRequest -> Cmd (ResponseData RegisterResponseError ())
register apiRoot model =
    Fetch.post
        { url = Fetch.register apiRoot
        , body = model |> registerRequestEncoder |> Http.jsonBody
        , expect = expectJsonWithError registerResponseErrorEnum.decoder (Decode.succeed ())
        }


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


type RegisterResponseError
    = UserAlreadyExists
    | InvalidRequest
    | PasswordHashingFailed
    | ValidationFailed


registerResponseErrorEnum : Enum RegisterResponseError
registerResponseErrorEnum =
    Enum.create
        [ ( "UserAlreadyExists", UserAlreadyExists )
        , ( "InvalidRequest", InvalidRequest )
        , ( "PasswordHashingFailed", PasswordHashingFailed )
        , ( "ValidationFailed", ValidationFailed )
        ]
