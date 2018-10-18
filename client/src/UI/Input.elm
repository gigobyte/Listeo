module UI.Input exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Types exposing (StyledElement)
import UI.Colors exposing (gray100)


view : StyledElement msg
view =
    styled input
        [ borderRadius <| px 5
        , padding <| px 7
        , border3 (px 1) solid gray100
        , fontFamilies [ "Museo-Sans" ]
        ]
