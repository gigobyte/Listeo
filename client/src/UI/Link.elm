module UI.Link exposing (LinkProps, view)

import Css exposing (..)
import Html.Styled exposing (..)
import Route
import UI.Colors exposing (blue150)
import Utils.Styles exposing (StyledElement)


type alias LinkProps =
    { to : Route.Route
    }


view : LinkProps -> StyledElement msg
view props attrs =
    styled a
        [ textDecoration none
        , color blue150
        ]
        (attrs
            ++ [ Route.href props.to ]
        )
