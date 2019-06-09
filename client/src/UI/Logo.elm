module UI.Logo exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Route
import Styles exposing (StyledElement)
import UI.Colors exposing (blue200)
import UI.Link as Link


viewContainer : StyledElement msg
viewContainer =
    styled span
        [ fontFamilies [ "Candara Regular" ]
        , color blue200
        , fontWeight bold
        , fontSize <| rem 3
        ]


view : Html msg
view =
    Link.view { to = Route.Home }
        []
        [ viewContainer []
            [ text "Listeo"
            ]
        ]
