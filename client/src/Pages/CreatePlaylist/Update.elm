module Pages.CreatePlaylist.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.CreatePlaylist.Model exposing (Model)


init : Model
init =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )
