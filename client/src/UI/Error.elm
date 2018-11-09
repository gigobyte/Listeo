module UI.Error exposing (text)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import UI.Colors exposing (redOrange100)
import Utils.Styles exposing (StyledElement)


type alias ErrorTextProps =
    { error : String }


text : ErrorTextProps -> StyledElement msg
text props attrs children =
    let
        baseStyle =
            [ color redOrange100
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
            baseStyle
                ++ (case props.error of
                        "" ->
                            []

                        _ ->
                            errorStyle
                   )
    in
    styled span style attrs ([ Html.Styled.text props.error ] ++ children)
