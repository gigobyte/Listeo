module UI.TagInput exposing (Tag, tagValue, view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onClick)
import UI.Colors exposing (..)
import UI.Icon as Icon
import UI.Input as Input
import Utils.Events exposing (onEnter)
import Utils.Styles exposing (StyledElement)


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


tagContent : StyledElement msg
tagContent =
    styled div
        [ display inlineBlock
        , color white
        , backgroundColor blue200
        , padding <| px 4
        , borderRadius4 (px 2) zero zero (px 2)
        ]


tagsContainer : StyledElement msg
tagsContainer =
    styled div
        [ displayFlex
        , paddingBottom <| px 10
        ]


tagContainer : StyledElement msg
tagContainer =
    styled div
        [ displayFlex
        ]


tagRemoveButton : StyledElement msg
tagRemoveButton =
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
    tagContainer []
        [ tagContent []
            [ text (tagValue tag)
            ]
        , tagRemoveButton [ onClick (onRemoveTag tag) ] [ Icon.times [] [] ]
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
            case props.value of
                "" ->
                    []

                _ ->
                    [ onEnter (props.onAddTag tagToBeAdded) ]
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
              , tagsContainer [] (List.map (viewTag props.onRemoveTag) props.tags)
              ]
            , children
            ]
        )
