module Pages.CreatePlaylist.Api exposing
    ( CreatePlaylistRequest
    , CreatePlaylistResponse
    , CreatePlaylistResponseError(..)
    , createPlaylist
    )

import Enum exposing (Enum)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Pages.CreatePlaylist.Model exposing (PlaylistPrivacy, PlaylistStyle, playlistPrivacyEnum, playlistStyleEnum)
import Utils.ErrorResponse exposing (ResponseData, expectJsonWithError)
import Utils.Fetch as Fetch exposing (ApiRoot, Token)


type alias CreatePlaylistRequest =
    { name : String
    , description : String
    , tags : List String
    , privacy : PlaylistPrivacy
    , style : PlaylistStyle
    }


type alias CreatePlaylistResponse =
    { playlistId : String }


type CreatePlaylistResponseError
    = InvalidRequest
    | ValidationFailed


createPlaylistResponseErrorEnum : Enum CreatePlaylistResponseError
createPlaylistResponseErrorEnum =
    Enum.create
        [ ( "InvalidRequest", InvalidRequest )
        , ( "ValidationFailed", ValidationFailed )
        ]


createPlaylist : ApiRoot -> Token -> CreatePlaylistRequest -> Cmd (ResponseData CreatePlaylistResponseError CreatePlaylistResponse)
createPlaylist apiRoot token req =
    Fetch.postWithAuth
        { url = Fetch.createPlaylist apiRoot
        , token = token
        , body = req |> createPlaylistRequestEncoder |> Http.jsonBody
        , expect =
            expectJsonWithError createPlaylistResponseErrorEnum.decoder createPlaylistResponseDecoder
        }


createPlaylistRequestEncoder : CreatePlaylistRequest -> Encode.Value
createPlaylistRequestEncoder req =
    Encode.object
        [ ( "name", Encode.string req.name )
        , ( "description", Encode.string req.description )
        , ( "tags", Encode.list Encode.string req.tags )
        , ( "privacy", playlistPrivacyEnum.encode req.privacy )
        , ( "style", playlistStyleEnum.encode req.style )
        ]


createPlaylistResponseDecoder : Decoder CreatePlaylistResponse
createPlaylistResponseDecoder =
    Decode.succeed (\x -> { playlistId = x })
        |> required "playlistId" string
