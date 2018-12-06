module UI.Logo exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Route
import UI.Colors exposing (blue150)
import UI.Link as Link
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled span
        [ fontFamilies [ "Candara Regular" ]
        , color blue150
        , fontWeight bold
        , fontSize <| rem 3
        ]


view : Html msg
view =
    Link.view { to = Route.Home }
        []
        [ container []
            [ text "Listeo"
            ]
        ]
