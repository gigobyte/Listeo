module Utils.Styles exposing
    ( StyledElement
    , stylesIfJust
    , stylesIfNotEmpty
    , stylesIfTrue
    )

import Html.Styled exposing (Attribute, Html)


type alias StyledElement msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


stylesIfJust : Maybe a -> List b -> List b
stylesIfJust cond styles =
    case cond of
        Just _ ->
            styles

        Nothing ->
            []


stylesIfNotEmpty : String -> List b -> List b
stylesIfNotEmpty cond styles =
    case cond of
        "" ->
            []

        _ ->
            styles


stylesIfTrue : Bool -> List b -> List b
stylesIfTrue cond styles =
    case cond of
        True ->
            styles

        False ->
            []
