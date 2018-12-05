module Pages.AddPlaylist.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.AddPlaylist.Model exposing (Model)


init : Model
init =
    { isOverlayShown = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlaylistOverlayShown ->
            ( { model | isOverlayShown = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )
