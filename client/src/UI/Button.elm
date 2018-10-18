module UI.Button exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Types exposing (StyledElement)
import UI.Colors exposing (blue100, white)


view : StyledElement msg
view =
    styled button
        [ borderRadius <| px 5
        , border <| px 0
        , backgroundColor blue100
        , color white
        , padding <| px 10
        , fontWeight bold
        , fontFamilies [ "Museo-Sans" ]
        ]
