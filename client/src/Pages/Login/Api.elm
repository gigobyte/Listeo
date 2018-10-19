module Pages.Login.Api exposing (LoginRequestData, makeLoginRequestModel)

import Pages.Login.Model exposing (Model)
import Pages.Login.Validation exposing (LoginValidationError, loginValidator)
import Validate exposing (fromValid, validate)


type alias LoginRequestData =
    { username : String
    , password : String
    }


makeLoginRequestModel : Model -> Maybe LoginRequestData
makeLoginRequestModel model =
    validate loginValidator model
        |> Result.map fromValid
        |> Result.map
            (\validatedModel ->
                { username = validatedModel.username
                , password = validatedModel.password
                }
            )
        |> Result.toMaybe
