module Pages.Login exposing (Model, Msg(..), init, update, view)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Types exposing (StyledElement)


type alias Model =
    { username : String
    , password : String
    }


type Msg
    = NoOp


init : Model
init =
    { username = ""
    , password = ""
    }


update : Model -> Msg -> Model
update model msg =
    model


container : StyledElement
container =
    styled div
        [ displayFlex
        , justifyContent center
        , alignItems center
        , height <| pct 70
        ]


view : Model -> Html Msg
view model =
    container [] [ text "Hello world from login" ]
