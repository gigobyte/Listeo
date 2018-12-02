module Pages.AddPlaylist.Update exposing (init, update)

import Msg exposing (Msg)
import Pages.AddPlaylist.Model exposing (Model)


init : Model
init =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
