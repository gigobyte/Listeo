module Utils.StyleTypes exposing (StyledElement)

import Html.Styled exposing (Attribute, Html)


type alias StyledElement msg =
    List (Attribute msg) -> List (Html msg) -> Html msg
