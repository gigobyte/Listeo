module Utils.ErrorResponse exposing (ErrorResponse, HttpError(..), ResponseData, expectJsonWithError)

import Http exposing (Expect, Response, expectStringResponse)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData)


type alias ResponseData e a =
    RemoteData (HttpError e) a


type alias ErrorResponse a =
    { error : a }


type HttpError a
    = BadUrl String
    | Timeout
    | NetworkError
    | Unauthorized
    | ServerError
    | BadResponse a
    | OtherUnsupportedStatus Int
    | BadBody String


decoder : Decoder a -> Decoder (ErrorResponse a)
decoder errorDecoder =
    Decode.succeed (\x -> { error = x })
        |> required "error" errorDecoder


expectJsonWithError : Decoder e -> Decoder a -> Expect (ResponseData e a)
expectJsonWithError errorDecoder successDecoder =
    let
        mapResponse : Response String -> Result (HttpError e) a
        mapResponse response =
            case response of
                Http.GoodStatus_ _ body ->
                    case Decode.decodeString successDecoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody (Decode.errorToString err))

                Http.BadStatus_ metadata body ->
                    case metadata.statusCode of
                        400 ->
                            case Decode.decodeString (decoder errorDecoder) body of
                                Ok value ->
                                    Err (BadResponse value.error)

                                Err err ->
                                    Err (BadBody (Decode.errorToString err))

                        401 ->
                            Err Unauthorized

                        500 ->
                            Err ServerError

                        _ ->
                            Err (OtherUnsupportedStatus metadata.statusCode)

                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError
    in
    expectStringResponse RemoteData.fromResult mapResponse
