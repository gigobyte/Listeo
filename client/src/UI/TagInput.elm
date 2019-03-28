module UI.TagInput exposing (Tag, tagValue, view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onClick)
import UI.Colors exposing (..)
import UI.Icon as Icon
import UI.Input as Input
import Utils.Events exposing (onEnter)
import Utils.Styles exposing (StyledElement, addIfNeeded)


type Tag
    = Tag { value : String, id : Int }


tagValue : Tag -> String
tagValue (Tag { value }) =
    value


type alias TagInputProps msg =
    { value : String
    , onAddTag : Tag -> msg
    , onRemoveTag : Tag -> msg
    , inputAttributes : List (Attribute msg)
    , tags : List Tag
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


viewTag : (Tag -> msg) -> Tag -> Html msg
viewTag onRemoveTag tag =
    viewTagContainer []
        [ viewTagContent []
            [ text (tagValue tag)
            ]
        , viewTagRemoveButton [ onClick (onRemoveTag tag) ] [ Icon.times [] [] ]
        ]


viewContainer : StyledElement msg
viewContainer =
    styled div []


view : TagInputProps msg -> StyledElement msg
view props attrs children =
    let
        tagToBeAdded =
            Tag { value = props.value, id = List.length props.tags }

        inputProps =
            List.concat
                [ addIfNeeded (props.value /= "") [ onEnter (props.onAddTag tagToBeAdded) ]
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
