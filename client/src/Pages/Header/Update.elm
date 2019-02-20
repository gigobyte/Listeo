module Pages.Header.Update exposing (init, update)

import Env exposing (Env)
import Msg exposing (Msg(..))
import Pages.Header.Model exposing (Model)
import Route


init : Model
init =
    { isOverlayShown = False
    }


update : Msg -> Model -> Env -> ( Model, Cmd Msg )
update msg model { pushUrl } =
    case msg of
        AddPlaylistOverlayShown ->
            ( { model | isOverlayShown = True }, Cmd.none )

        CreateNewPlaylistSelected ->
            ( { model | isOverlayShown = False }, pushUrl Route.CreatePlaylist )

        AddPlaylistModalClosed ->
            ( { model | isOverlayShown = False }, Cmd.none )

        _ ->
            ( model, Cmd.none )
