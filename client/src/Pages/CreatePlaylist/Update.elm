module Pages.CreatePlaylist.Update exposing (init, update)

import List.Extra as List
import Msg exposing (Msg(..))
import Pages.CreatePlaylist.Model exposing (Model, PlaylistPrivacy(..), PlaylistStyle(..))
import UI.TagInput exposing (tagValue)


init : Model
init =
    { playlistName = ""
    , playlistTagInput = ""
    , playlistTags = []
    , playlistPrivacy = Public
    , playlistStyle = Unordered
    , showErrors = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaylistNameUpdated value ->
            ( { model | playlistName = value }, Cmd.none )

        PlaylistTagInputUpdated value ->
            ( { model | playlistTagInput = String.trim value }, Cmd.none )

        PlaylistTagAdded tag ->
            ( { model
                | playlistTags = List.uniqueBy tagValue (model.playlistTags ++ [ tag ])
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
            case Nothing of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | showErrors = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )
