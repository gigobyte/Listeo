module UI.Error exposing (viewText)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import UI.Colors exposing (crimson100)
import Utils.Styles exposing (StyledElement, addIfNeeded)


type alias ErrorTextProps =
    { error : String }


viewText : ErrorTextProps -> StyledElement msg
viewText props attrs children =
    let
        baseStyle =
            [ color crimson100
            , maxHeight zero
            , opacity zero
            , transition [ Transitions.maxHeight 3000, Transitions.opacity 1000 ]
            ]

        errorStyle =
            [ marginBottom <| px 10
            , maxHeight <| px 999
            , opacity <| int 1
            ]

        style =
            List.concat
                [ baseStyle
                , addIfNeeded (props.error /= "") errorStyle
                ]
    in
    styled span style attrs (Html.Styled.text props.error :: children)
