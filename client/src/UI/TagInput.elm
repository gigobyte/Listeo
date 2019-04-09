module UI.TagInput exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onClick)
import UI.Colors exposing (..)
import UI.Icon as Icon
import UI.Input as Input
import Utils.Events exposing (onEnter)
import Utils.Styles exposing (StyledElement, addIfNeeded)


type alias TagInputProps msg =
    { value : String
    , onAddTag : String -> msg
    , onRemoveTag : String -> msg
    , inputAttributes : List (Attribute msg)
    , tags : List String
    }


viewTagContent : StyledElement msg
viewTagContent =
    styled div
        [ display inlineBlock
        , color white
        , backgroundColor blue200
        , padding <| px 4
        , borderRadius4 (px 2) zero zero (px 2)
        ]


viewTagsContainer : StyledElement msg
viewTagsContainer =
    styled div
        [ displayFlex
        , paddingBottom <| px 10
        ]


viewTagContainer : StyledElement msg
viewTagContainer =
    styled div
        [ displayFlex
        ]


viewTagRemoveButton : StyledElement msg
viewTagRemoveButton =
    styled button
        [ backgroundColor blue300
        , border zero
        , cursor pointer
        , marginRight <| px 7
        , height <| px 26
        , color white
        , borderRadius4 zero (px 2) (px 2) zero
        , focus
            [ outline none
            ]
        ]


viewTag : (String -> msg) -> String -> Html msg
viewTag onRemoveTag tag =
    viewTagContainer []
        [ viewTagContent []
            [ text tag
            ]
        , viewTagRemoveButton [ onClick (onRemoveTag tag) ] [ Icon.times [] [] ]
        ]


viewContainer : StyledElement msg
viewContainer =
    styled div []


view : TagInputProps msg -> StyledElement msg
view props attrs children =
    let
        inputProps =
            List.concat
                [ addIfNeeded (props.value /= "") [ onEnter (props.onAddTag props.value) ]
                ]
    in
    viewContainer
        attrs
        (List.concat
            [ [ Input.view
                    { inputAttributes = props.inputAttributes ++ [ value props.value ]
                    , validationError = Nothing
                    }
                    inputProps
                    []
              , if List.length props.tags > 0 then
                    viewTagsContainer [] (List.map (viewTag props.onRemoveTag) props.tags)

                else
                    text ""
              ]
            , children
            ]
        )
