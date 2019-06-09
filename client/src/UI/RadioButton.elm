module UI.RadioButton exposing (view)

import Css exposing (..)
import Css.Global exposing (adjacentSiblings, children, typeSelector)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (checked, type_)
import Html.Styled.Events exposing (onClick)
import Styles exposing (StyledElement)
import UI.Colors exposing (blue100)


type alias Props msg =
    { isChecked : Bool
    , label : String
    , onCheck : msg
    }


viewInput : StyledElement msg
viewInput =
    styled input
        [ display none
        ]


viewLabel : StyledElement msg
viewLabel =
    styled label
        [ position relative
        , display inlineBlock
        , paddingLeft <| px 25
        , height <| px 22
        , children
            [ typeSelector "input"
                [ Css.checked
                    [ adjacentSiblings
                        [ typeSelector "span"
                            [ after
                                [ property "content" "''"
                                , height <| px 10
                                , width <| px 10
                                , backgroundColor blue100
                                , position absolute
                                , borderRadius <| pct 50
                                , left <| pct 50
                                , top <| pct 50
                                , transform <| translate2 (pct -50) (pct -50)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewCircle : StyledElement msg
viewCircle =
    styled span
        [ display inlineBlock
        , width <| px 18
        , height <| px 18
        , position absolute
        , left zero
        , top <| px -3
        , borderRadius <| pct 50
        , border3 (px 2) solid blue100
        ]


view : Props msg -> StyledElement msg
view props _ _ =
    viewLabel []
        [ text props.label
        , viewInput [ type_ "radio", checked props.isChecked, onClick props.onCheck ] []
        , viewCircle [] []
        ]
