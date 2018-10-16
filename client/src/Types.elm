module Types exposing (StyledElement)

import Main exposing (Msg)


type alias StyledElement =
    List (Attribute Msg) -> List (Html Msg) -> Html Msg
