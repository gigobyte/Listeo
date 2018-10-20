module UI.Input exposing (InputProps, view)

import Css exposing (..)
import Css.Transitions as Transitions exposing (easeIn, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (type_)
import Maybe.Extra as Maybe
import UI.Colors exposing (gray100, redOrange100, whiteGray100)
import Utils.StyleTypes exposing (StyledElement)


type alias InputProps msg =
    { validationError : Maybe String
    , inputAttributes : List (Attribute msg)
    }


viewError : Maybe String -> Html msg
viewError errorText =
    let
        baseStyle =
            [ color redOrange100
            , textAlign center
            , maxHeight zero
            , transition [ Transitions.maxHeight 3000, Transitions.opacity 1000 ]
            , opacity zero
            ]

        errorStyle =
            [ marginBottom <| px 10
            , maxHeight <| px 999
            , opacity <| int 1
            ]

        style =
            baseStyle
                ++ (case errorText of
                        Just _ ->
                            errorStyle

                        _ ->
                            []
                   )
    in
    styled div style [] [ text (errorText |> Maybe.withDefault "") ]


viewInput : Bool -> StyledElement msg
viewInput hasError =
    let
        errorStyle =
            if hasError then
                [ marginBottom <| px 5
                , borderColor redOrange100
                ]

            else
                []
    in
    styled input
        ([ borderRadius <| px 5
         , padding <| px 7
         , fontSize <| px 16
         , marginBottom <| px 10
         , border3 (px 1) solid gray100
         , transition [ Transitions.borderColor 1000 ]
         , fontFamilies [ "Museo-Sans" ]
         , backgroundColor whiteGray100
         ]
            ++ errorStyle
        )


view : InputProps msg -> StyledElement msg
view props attrs children =
    styled div
        []
        attrs
        ([ viewInput (Maybe.isJust props.validationError) props.inputAttributes []
         , viewError props.validationError
         ]
            ++ children
        )
