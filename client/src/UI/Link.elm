module UI.Link exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Routes
import Utils.StyleTypes exposing (StyledElement)


type alias LinkProps msg =
    { to : Routes.Route
    , attributes : List (Attribute msg)
    }


view : LinkProps msg -> List (Html msg) -> Html msg
view props =
    styled a
        [ textDecoration none
        ]
        (props.attributes
        ++ [ Routes.href props.to ])
