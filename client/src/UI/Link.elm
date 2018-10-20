module UI.Link exposing (LinkProps, view)

import Css exposing (..)
import Html.Styled exposing (..)
import Routes
import Utils.StyleTypes exposing (StyledElement)


type alias LinkProps =
    { to : Routes.Route
    }


view : LinkProps -> StyledElement msg
view props attrs =
    styled a
        [ textDecoration none
        ]
        (attrs
            ++ [ Routes.href props.to ]
        )
