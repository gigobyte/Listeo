module UI.Icon exposing (plusCircle)

import Css exposing (..)
import Html.Styled exposing (Html, i, styled)
import Html.Styled.Attributes exposing (class)
import Utils.Styles exposing (StyledElement)


icon : StyledElement msg
icon =
    styled i
        [ width <| pct 100
        ]


plusCircle : StyledElement msg
plusCircle attrs =
    icon (attrs ++ [ class "fas fa-plus-circle" ])
