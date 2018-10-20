module UI.Logo exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import UI.Colors exposing (blue150)


view : Html msg
view =
    styled span
        [ fontFamilies [ "Candara Regular" ]
        , color blue150
        , fontWeight bold
        , fontSize <| rem 3
        ]
        []
        [ text "Listeo" ]
