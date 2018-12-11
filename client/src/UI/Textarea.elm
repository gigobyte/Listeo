module UI.Textarea exposing (view)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (type_)
import Maybe.Extra as Maybe
import UI.Colors exposing (gray100, redOrange100, whiteGray100)
import Utils.Styles exposing (StyledElement, stylesIfJust, stylesIfTrue)


type alias TextareaProps msg =
    { validationError : Maybe String
    , textareaAttributes : List (Attribute msg)
    }


viewError : Maybe String -> Html msg
viewError errorText =
    let
        baseStyle =
            [ color redOrange100
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
            baseStyle ++ stylesIfJust errorText errorStyle
    in
    styled div style [] [ text (errorText |> Maybe.withDefault "") ]


viewInput : Bool -> StyledElement msg
viewInput hasError =
    let
        errorStyle =
            stylesIfTrue hasError
                [ marginBottom <| px 5
                , borderColor redOrange100
                ]
    in
    styled textarea
        ([ borderRadius <| px 2
         , padding <| px 7
         , width <| px 200
         , paddingLeft <| px 15
         , fontSize <| px 16
         , marginBottom <| px 10
         , height <| px 75
         , border zero
         , transition [ Transitions.borderColor 1000 ]
         , fontFamilies [ "Museo-Sans" ]
         , backgroundColor whiteGray100
         , boxShadow5 (px 1) (px 2) (px 3) zero gray100
         , focus
            [ outline zero
            , boxShadow5 (px 1) (px 2) (px 3) (px 1) gray100
            ]
         ]
            ++ errorStyle
        )


view : TextareaProps msg -> StyledElement msg
view props attrs children =
    styled div
        [ textAlign center
        ]
        attrs
        ([ viewInput (Maybe.isJust props.validationError) props.textareaAttributes []
         , viewError props.validationError
         ]
            ++ children
        )
