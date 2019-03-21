module Pages.CreatePlaylist.Api exposing
    ( CreatePlaylistRequest
    , CreatePlaylistResponse
    , createPlaylist
    )

import Http exposing (expectJson)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Pages.CreatePlaylist.Model exposing (PlaylistPrivacy, PlaylistStyle, playlistPrivacyToString, playlistStyleToString)
import RemoteData exposing (WebData)
import Utils.Fetch as Fetch exposing (ApiRoot, Token)


type alias CreatePlaylistRequest =
    { name : String
    , tags : List String
    , privacy : PlaylistPrivacy
    , style : PlaylistStyle
    }


type CreatePlaylistResponseError
    = ServerError


type CreatePlaylistResponse
    = ErrorResponse { errorDescription : CreatePlaylistResponseError }
    | SuccessResponse { playlistId : String }


createPlaylist : ApiRoot -> Token -> CreatePlaylistRequest -> Cmd (WebData CreatePlaylistResponse)
createPlaylist apiRoot token req =
    Fetch.postWithAuth
        { url = Fetch.createPlaylist apiRoot
        , token = token
        , body = req |> createPlaylistRequestEncoder |> Http.jsonBody
        , expect =
            expectJson RemoteData.fromResult
                (Decode.oneOf
                    [ successResponseDecoder, errorResponseDecoder ]
                )
        }


createPlaylistRequestEncoder : CreatePlaylistRequest -> Encode.Value
createPlaylistRequestEncoder req =
    Encode.object
        [ ( "name", Encode.string req.name )
        , ( "tags", Encode.list Encode.string req.tags )
        , ( "privacy", Encode.string <| playlistPrivacyToString req.privacy )
        , ( "style", Encode.string <| playlistStyleToString req.style )
        ]


successResponseDecoder : Decoder CreatePlaylistResponse
successResponseDecoder =
    Decode.succeed (\x -> { playlistId = x })
        |> required "playlistId" string
        |> Decode.andThen (Decode.succeed << SuccessResponse)


errorResponseDecoder : Decoder CreatePlaylistResponse
errorResponseDecoder =
    Decode.succeed (\x -> { errorDescription = x })
        |> required "errorDescription" createPlaylistErrorDecoder
        |> Decode.andThen (Decode.succeed << ErrorResponse)


createPlaylistErrorDecoder : Decoder CreatePlaylistResponseError
createPlaylistErrorDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    _ ->
                        Decode.succeed ServerError
            )
