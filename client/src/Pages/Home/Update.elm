module Pages.Home.Update exposing (init, update)

import Session exposing (Session)
import Msg exposing (Msg)
import Pages.Home.Model exposing (Model)


init : Model
init =
    {}


update : Msg -> Model -> Session -> ( Model, Cmd Msg )
update _ model _ =
    ( model, Cmd.none )
