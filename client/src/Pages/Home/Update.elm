module Pages.Home.Update exposing (init, update)

import Env exposing (Env)
import Msg exposing (Msg)
import Pages.Home.Model exposing (Model)


init : Model
init =
    {}


update : Msg -> Model -> Env -> ( Model, Cmd Msg )
update _ model _ =
    ( model, Cmd.none )
