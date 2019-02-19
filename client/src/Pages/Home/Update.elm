module Pages.Home.Update exposing (init, update)

import Msg exposing (Msg)
import Pages.Home.Model exposing (Model)


init : Model
init =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )
