module Pages.CreatePlaylist.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.CreatePlaylist.Model exposing (Model)
import UI.TagInput exposing (tagValue)


init : Model
init =
    { playlistName = ""
    , playlistTagInput = ""
    , playlistTags = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaylistNameUpdated value ->
            ( { model | playlistName = value }, Cmd.none )

        PlaylistTagInputUpdated value ->
            ( { model | playlistTagInput = value }, Cmd.none )

        PlaylistTagAdded tag ->
            ( { model
                | playlistTags = List.append model.playlistTags [ tag ]
                , playlistTagInput = ""
              }
            , Cmd.none
            )

        PlaylistTagRemoved tag ->
            ( { model
                | playlistTags = List.filter (\x -> tagValue x /= tagValue tag) model.playlistTags
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
