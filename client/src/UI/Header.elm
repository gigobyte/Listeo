module UI.Header exposing (view)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Route
import UI.Colors exposing (blue150, blue50)
import UI.Link as Link
import UI.Logo as Logo
import Utils.Styles exposing (StyledElement)


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
        , transition [ Transitions.color 500 ]
        , hover
            [ color blue50
            ]
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
            [ navItem { to = Route.Login } [] [ text "Sign In" ]
            , navItem { to = Route.Register } [] [ text "Register" ]
            , navItem { to = Route.About } [] [ text "About" ]
            ]
        ]
