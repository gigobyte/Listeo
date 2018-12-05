module UI.Button exposing (view)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import UI.Colors exposing (blue100, blue200, white)
import Utils.Styles exposing (StyledElement)


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
            ]
        , active
            [ transform (scale 0.95)
            ]
        , disabled
            [ cursor notAllowed
            , opacity <| num 0.7
            ]
        ]
