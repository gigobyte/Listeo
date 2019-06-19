module Pages.ViewPlaylist exposing (Model, Msg, init, toSession, update, updateSession, view)

-- MODEL

import ErrorResponse exposing (ResponseData, expectJsonWithError)
import Fetch exposing (ApiRoot, Token)
import Iso8601
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)
import Pages.Playlist.PlaylistPrivacy as PlaylistPrivacy exposing (PlaylistPrivacy)
import Pages.Playlist.PlaylistStyle as PlaylistStyle exposing (PlaylistStyle)
import RemoteData exposing (RemoteData(..))
import Session exposing (Session)
import Styles exposing (StyledDocument)
import Time


type alias Model =
    { session : Session
    , playlist : ResponseData () Playlist
    }


type alias Playlist =
    { id : String
    , style : PlaylistStyle
    , name : String
    , privacy : PlaylistPrivacy
    , createdOn : Time.Posix
    , tags : List PlaylistTag
    }


type alias PlaylistTag =
    { id : String
    , name : String
    , createdOn : Time.Posix
    }


init : Session -> String -> ( Model, Cmd Msg )
init session playlistId =
    ( { session = session
      , playlist = NotAsked
      }
    , getPlaylist session.apiRoot session.token playlistId |> Cmd.map GetPlaylist
    )



-- VIEW


view : Model -> StyledDocument Msg
view model =
    { title =
        case model.playlist of
            Success playlist ->
                playlist.name ++ " - Listeo"

            _ ->
                "Listeo"
    , body = []
    }



-- UPDATE


type Msg
    = GetPlaylist (ResponseData () Playlist)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPlaylist _ ->
            ( model, Cmd.none )



-- API


getPlaylist : ApiRoot -> Token -> String -> Cmd (ResponseData () Playlist)
getPlaylist apiRoot token playlistId =
    Fetch.getWithAuth
        { url = Fetch.getPlaylist apiRoot playlistId
        , token = token
        , expect =
            expectJsonWithError (Decode.succeed ()) playlistDecoder
        }


playlistDecoder : Decoder Playlist
playlistDecoder =
    Decode.succeed Playlist
        |> required "id" string
        |> required "style" PlaylistStyle.decoder
        |> required "name" string
        |> required "privacy" PlaylistPrivacy.decoder
        |> required "createdOn" Iso8601.decoder
        |> required "tags" (list playlistTagDecoder)


playlistTagDecoder : Decoder PlaylistTag
playlistTagDecoder =
    Decode.succeed PlaylistTag
        |> required "id" string
        |> required "name" string
        |> required "createdOn" Iso8601.decoder



-- EXPORTS


toSession : Model -> Session
toSession model =
    model.session


updateSession : Session -> Model -> Model
updateSession newSession model =
    { model | session = newSession }
