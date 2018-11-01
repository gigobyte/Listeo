module Pages.Register.Api exposing (RegisterRequestData, makeRegisterRequestModel)

import Pages.Register.Model exposing (Model)
import Pages.Register.Validation exposing (RegisterValidationError, registerValidator)
import Validate exposing (fromValid, validate)


type alias RegisterRequestData =
    { username : String
    , password : String
    }


makeRegisterRequestModel : Model -> Maybe RegisterRequestData
makeRegisterRequestModel model =
    validate registerValidator model
        |> Result.map fromValid
        |> Result.map
            (\validatedModel ->
                { username = validatedModel.username
                , password = validatedModel.password
                }
            )
        |> Result.toMaybe
