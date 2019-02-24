module UI.RadioButton exposing (view)

-- import UI.Colors exposing (gray300, white)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (checked, type_)
import Html.Styled.Events exposing (onClick)
import Utils.Styles exposing (StyledElement)


type alias Props msg =
    { isChecked : Bool
    , label : String
    , onToggle : msg
    }



-- styledInput : StyledElement msg
-- styledInput =
--     styled input
--         [ position absolute
--         , left <| px -9999
--         ]
-- styledLabel : StyledElement msg
-- styledLabel =
--     styled label
--         [ displayFlex
--         , position relative
--         , paddingLeft <| px 28
--         , before
--             [ property "content" "''"
--             , position absolute
--             , left zero
--             , top zero
--             , width <| px 18
--             , height <| px 18
--             , borderRadius <| pct 100
--             , backgroundColor white
--             , border3 (px 1) solid gray300
--             ]
--         ]


view : Props msg -> StyledElement msg
view props _ _ =
    label []
        [ input [ type_ "radio", checked props.isChecked, onClick props.onToggle ] []
        , text props.label
        ]
