module Utils.Validation exposing (Problems(..), getErrorForField, hasFailed, mapProblems)

import List.Extra as List


type Problems field error
    = NotShown
    | Shown (List ( field, error ))


hasFailed : Problems field error -> Bool
hasFailed problems =
    case problems of
        NotShown ->
            False

        Shown [] ->
            False

        Shown _ ->
            True


mapProblems : (List ( field, error ) -> List ( field, error )) -> Problems field error -> Problems field error
mapProblems transform problems =
    case problems of
        NotShown ->
            problems

        Shown errors ->
            Shown (transform errors)


getErrorForField : a -> Problems a b -> Maybe b
getErrorForField field problems =
    case problems of
        NotShown ->
            Nothing

        Shown errors ->
            errors
                |> List.find (\( x, _ ) -> x == field)
                |> Maybe.map Tuple.second
