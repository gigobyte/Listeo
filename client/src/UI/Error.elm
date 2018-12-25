module UI.Error exposing (text)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import UI.Colors exposing (crimson100)
import Utils.Styles exposing (StyledElement, stylesIfNotEmpty)


type alias ErrorTextProps =
    { error : String }


text : ErrorTextProps -> StyledElement msg
text props attrs children =
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
            baseStyle ++ stylesIfNotEmpty props.error errorStyle
    in
    styled span style attrs ([ Html.Styled.text props.error ] ++ children)
