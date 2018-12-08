module Pages.Header.Update exposing (init, update)

import Browser.Navigation as Nav
import Msg exposing (Msg(..))
import Pages.Header.Model exposing (Model)
import Route exposing (pushUrl)


type alias Context =
    { key : Nav.Key
    }


init : Model
init =
    { isOverlayShown = False
    }


update : Msg -> Model -> Context -> ( Model, Cmd Msg )
update msg model ctx =
    case msg of
        AddPlaylistOverlayShown ->
            ( { model | isOverlayShown = True }, Cmd.none )

        CreateNewPlaylistSelected ->
            ( { model | isOverlayShown = False }, pushUrl ctx.key Route.CreatePlaylist )

        AddPlaylistModalClosed ->
            ( { model | isOverlayShown = False }, Cmd.none )

        _ ->
            ( model, Cmd.none )
