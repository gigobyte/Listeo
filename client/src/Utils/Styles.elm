module Utils.Styles exposing
    ( StyledDocument
    , StyledElement
    , addIfNeeded
    , toUnstyledDocument
    )

import Browser exposing (Document)
import Html.Styled exposing (Attribute, Html, toUnstyled)


type alias StyledElement msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type alias StyledDocument msg =
    { title : String
    , body : List (Html msg)
    }


addIfNeeded : Bool -> List b -> List b
addIfNeeded isNeeded attrs =
    if isNeeded then
        attrs

    else
        []


toUnstyledDocument : StyledDocument msg -> Document msg
toUnstyledDocument doc =
    { title = doc.title
    , body = List.map toUnstyled doc.body
    }
