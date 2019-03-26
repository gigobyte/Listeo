module UI.Container exposing (viewCentered, viewFullHeight)

import Css exposing (..)
import Html.Styled exposing (..)
import Utils.Styles exposing (StyledElement)


viewFullHeight : StyledElement msg -> StyledElement msg
viewFullHeight el =
    styled el
        [ height <| pct 100 ]


viewCentered : StyledElement msg -> StyledElement msg
viewCentered el =
    styled el
        [ displayFlex
        , justifyContent center
        , alignItems center
        , flexDirection column
        ]
