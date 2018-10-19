module Pages.Login.Selectors exposing (getValidationErrors)

import Pages.Login.Model exposing (Model)
import Pages.Login.Validation exposing (LoginValidationError, LoginField, loginValidator)
import Result.Extra as Result
import Validate exposing (validate)


getValidationErrors : Model -> List ( LoginField, LoginValidationError )
getValidationErrors model =
    if model.showErrors then
        validate loginValidator model
            |> Result.map (always [])
            |> Result.merge

    else
        []
