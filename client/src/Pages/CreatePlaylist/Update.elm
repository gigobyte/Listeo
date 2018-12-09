module Pages.CreatePlaylist.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.CreatePlaylist.Model exposing (Model)


init : Model
init =
    { playlistName = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaylistNameUpdated value ->
            ( { model | playlistName = value }, Cmd.none )

        _ ->
            ( model, Cmd.none )
