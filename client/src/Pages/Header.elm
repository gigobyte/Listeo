module Pages.Header exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Routes
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
        , paddingRight <| pct 5
        , fontSize <| rem 1.1
        ]


logo : StyledElement msg
logo =
    styled div
        [ padding <| px 20
        ]


view : Html msg
view =
    container []
        [ logo [] [ Logo.view ]
        , nav []
            [ Link.view { to = Routes.Login } [] [ text "Sign In" ]
            , Link.view { to = Routes.Register } [] [ text "Register" ]
            , Link.view { to = Routes.About } [] [ text "About" ]
            ]
        ]
