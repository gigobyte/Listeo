module Pages.Register.Selectors exposing (getValidationErrors)

import Pages.Register.Model exposing (Model)
import Pages.Register.Validation exposing (RegisterField, RegisterValidationError, registerValidator)
import Result.Extra as Result
import Validate exposing (validate)


getValidationErrors : Model -> List ( RegisterField, RegisterValidationError )
getValidationErrors model =
    if model.showErrors then
        validate registerValidator model
            |> Result.map (always [])
            |> Result.merge

    else
        []
