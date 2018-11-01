module UI.Container exposing (centered, fullHeight)

import Css exposing (..)
import Html.Styled exposing (..)
import Utils.StyleTypes exposing (StyledElement)


fullHeight : StyledElement msg -> StyledElement msg
fullHeight el =
    styled el
        [ height <| pct 100 ]


centered : StyledElement msg -> StyledElement msg
centered el =
    styled el
        [ displayFlex
        , justifyContent center
        , alignItems center
        , flexDirection column
        ]
