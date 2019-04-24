module Pages.CreatePlaylist.Update exposing (init, update)

import List.Extra as List
import Msg exposing (Msg(..))
import Pages.CreatePlaylist.Api as Api
import Pages.CreatePlaylist.Model exposing (Model, PlaylistPrivacy(..), PlaylistStyle(..))
import Pages.CreatePlaylist.Validation exposing (makeCreatePlaylistRequestModel)
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session)


init : Model
init =
    { playlistName = ""
    , playlistDescription = ""
    , playlistTagInput = ""
    , playlistTags = []
    , playlistPrivacy = Public
    , playlistStyle = Unordered
    , showErrors = False
    }


update : Msg -> Model -> Session -> ( Model, Cmd Msg )
update msg model session =
    case msg of
        PlaylistNameUpdated value ->
            ( { model | playlistName = value }, Cmd.none )

        PlaylistTagInputUpdated value ->
            if String.endsWith "," value then
                update (PlaylistTagAdded <| String.dropRight 1 value) model session

            else
                ( { model | playlistTagInput = String.trim value }, Cmd.none )

        PlaylistTagAdded tag ->
            ( { model
                | playlistTags = List.unique (model.playlistTags ++ [ tag ])
                , playlistTagInput = ""
              }
            , Cmd.none
            )

        PlaylistTagRemoved tag ->
            ( { model
                | playlistTags = List.remove tag model.playlistTags
              }
            , Cmd.none
            )

        PlaylistPrivacySelected option ->
            ( { model | playlistPrivacy = option }, Cmd.none )

        PlaylistStyleSelected option ->
            ( { model | playlistStyle = option }, Cmd.none )

        CreatePlaylistAttempted ->
            case makeCreatePlaylistRequestModel model of
                Just request ->
                    ( model, Api.createPlaylist session.apiRoot session.token request |> Cmd.map CreatePlaylist )

                Nothing ->
                    ( { model | showErrors = True }, Cmd.none )

        CreatePlaylist (Success { playlistId }) ->
            ( model, session.pushUrl (Route.ViewPlaylist playlistId) )

        _ ->
            ( model, Cmd.none )
