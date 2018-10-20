module Pages.Header exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Routes
import UI.Colors exposing (blue150)
import UI.Link as Link
import UI.Logo as Logo
import Utils.StyleTypes exposing (StyledElement)


container : StyledElement msg
container =
    styled div
        [ displayFlex
        , justifyContent spaceBetween
        ]


nav : StyledElement msg
nav =
    styled div
        [ textTransform uppercase
        , displayFlex
        , alignItems center
        , paddingRight <| pct 4
        , fontSize <| rem 1.1
        , fontWeight bold
        ]


navItem : Link.LinkProps -> StyledElement msg
navItem props =
    styled (Link.view props)
        [ padding2 zero (px 10)
        ]


logo : StyledElement msg
logo =
    styled div
        [ padding <| px 20
        , paddingLeft <| pct 4
        ]


view : Html msg
view =
    container []
        [ logo [] [ Logo.view ]
        , nav []
            [ navItem { to = Routes.Login } [] [ text "Sign In" ]
            , navItem { to = Routes.Register } [] [ text "Register" ]
            , navItem { to = Routes.About } [] [ text "About" ]
            ]
        ]
