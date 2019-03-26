module Utils.Styles exposing
    ( StyledElement
    , addIfNeeded
    )

import Html.Styled exposing (Attribute, Html)


type alias StyledElement msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


addIfNeeded : Bool -> List b -> List b
addIfNeeded isNeeded attrs =
    if isNeeded then
        attrs

    else
        []
