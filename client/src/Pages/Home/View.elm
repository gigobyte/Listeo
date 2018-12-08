module Pages.Home.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Msg exposing (Msg)


type alias Props =
    {}


view : Props -> Html Msg
view props =
    div []
        [ text "Home page"
        ]
