module UI.Button exposing (view)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import UI.Colors exposing (blue100, blue200, white)
import Utils.StyleTypes exposing (StyledElement)


view : StyledElement msg
view =
    styled button
        [ borderRadius <| px 44
        , border zero
        , backgroundImage <| linearGradient2 (deg 310) (stop blue200) (stop blue100) []
        , transition [ Transitions.transform 300 ]
        , color white
        , padding2 (px 10) (px 20)
        , fontWeight bold
        , fontFamilies [ "Museo-Sans" ]
        , cursor pointer
        , focus
            [ outline none
            , boxShadow5 zero zero (px 3) (px 1) blue100
            ]
        , active
            [ transform (scale 0.95)
            ]
        ]
