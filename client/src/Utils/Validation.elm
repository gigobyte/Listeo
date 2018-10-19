module Utils.Validation exposing (getErrorForField)

import List.Extra as List


getErrorForField : a -> List ( a, b ) -> Maybe b
getErrorForField field =
    List.find (\( x, _ ) -> x == field)
        >> Maybe.map Tuple.second
