module Utils.Events exposing (onEnter)

import Html.Styled exposing (Attribute)
import Html.Styled.Events exposing (..)
import Json.Decode as Decode


onEnter : msg -> Attribute msg
onEnter onEnterAction =
    on "keyup" <|
        Decode.andThen
            (\keyCode ->
                if keyCode == 13 then
                    Decode.succeed onEnterAction

                else
                    Decode.fail (String.fromInt keyCode)
            )
            keyCode
