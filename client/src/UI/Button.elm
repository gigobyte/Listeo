module UI.Button exposing (view)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Styles exposing (StyledElement)
import UI.Colors exposing (blue300, blue400, white)


view : StyledElement msg
view =
    styled button
        [ borderRadius <| px 44
        , border zero
        , backgroundImage <| linearGradient2 (deg 310) (stop blue400) (stop blue300) []
        , transition [ Transitions.transform 300 ]
        , color white
        , padding2 (px 10) (px 20)
        , fontWeight bold
        , fontSize <| rem 1
        , fontFamilies [ "Museo-Sans" ]
        , cursor pointer
        , focus
            [ outline none
            ]
        , active
            [ transform (scale 0.95)
            ]
        , disabled
            [ cursor notAllowed
            , opacity <| num 0.7
            ]
        ]
