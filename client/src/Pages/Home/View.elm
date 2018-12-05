module Pages.Home.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Msg exposing (Msg)
import Pages.Home.Model exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ text "Home page"
        ]
