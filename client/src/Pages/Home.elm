module Pages.Home exposing (Model, Msg(..), init, update, view)

import Html exposing (..)


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    {}


update : Model -> Msg -> Model
update model msg =
    model


view : Model -> Html Msg
view model =
    text "Hello world from home"
