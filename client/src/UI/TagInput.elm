module UI.TagInput exposing (Tag, tagValue, view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import UI.Colors exposing (..)
import UI.Icon as Icon
import UI.Input as Input
import Utils.Events exposing (onEnter)
import Utils.Styles exposing (StyledElement)


type Tag
    = Tag { value : String, id : Int }


tagValue : Tag -> String
tagValue (Tag { value, id }) =
    value


type alias TagInputProps msg =
    { validationError : Maybe String
    , placeholder : String
    , value : String
    , onInput : String -> msg
    , onAddTag : Tag -> msg
    , onRemoveTag : Tag -> msg
    , tags : List Tag
    }


tagContent : StyledElement msg
tagContent =
    styled div
        [ display inlineBlock
        , color white
        , backgroundColor blue150
        , padding <| px 4
        , borderRadius4 (px 2) zero zero (px 2)
        ]


tagRemoveButton : StyledElement msg
tagRemoveButton =
    styled button
        [ backgroundColor blue125
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


viewTag : (Tag -> msg) -> Tag -> Html msg
viewTag onRemoveTag tag =
    span []
        [ tagContent [] [ text (tagValue tag) ], tagRemoveButton [ onClick (onRemoveTag tag) ] [ Icon.times [] [] ] ]


view : TagInputProps msg -> StyledElement msg
view props attrs children =
    let
        tagToBeAdded =
            Tag { value = props.value, id = List.length props.tags }
    in
    styled div
        []
        attrs
        ([ Input.view
            { inputAttributes =
                [ placeholder props.placeholder
                , onInput props.onInput
                , value props.value
                ]
            , validationError = props.validationError
            }
            [ onEnter (props.onAddTag tagToBeAdded) ]
            []
         , div [] (List.map (viewTag props.onRemoveTag) props.tags)
         ]
            ++ children
        )
