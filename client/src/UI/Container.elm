module UI.Container exposing (centered, fullHeight)

import Css exposing (..)
import Html.Styled exposing (..)
import Utils.StyleTypes exposing (StyledElement)


fullHeight : StyledElement msg
fullHeight =
    styled div
        [ height <| pct 100 ]


centered : StyledElement msg
centered =
    styled div
        [ displayFlex
        , justifyContent center
        , alignItems center
        , flexDirection column
        ]
