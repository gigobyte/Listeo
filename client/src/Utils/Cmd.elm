module Utils.Cmd exposing (dispatch)

import Task


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform identity
