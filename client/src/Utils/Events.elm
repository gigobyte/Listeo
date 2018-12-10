module Utils.Events exposing (onEnter)

import Html.Styled exposing (Attribute)
import Html.Styled.Events exposing (..)
import Json.Decode as Json


onEnter : msg -> Attribute msg
onEnter onEnterAction =
    on "keyup" <|
        Json.andThen
            (\keyCode ->
                if keyCode == 13 then
                    Json.succeed onEnterAction

                else
                    Json.fail (String.fromInt keyCode)
            )
            keyCode
