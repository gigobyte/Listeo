module Pages.Login.Validation exposing
    ( LoginField(..)
    , LoginValidationError(..)
    , errToString
    , loginValidator
    , makeLoginRequestModel
    )

import Pages.Login.Api exposing (LoginRequest)
import Pages.Login.Model exposing (Model)
import Validate exposing (Validator, fromValid, ifBlank, ifTrue, validate)


type LoginValidationError
    = UsernameMissing
    | PasswordMissing


type LoginField
    = Username
    | Password


errToString : LoginValidationError -> String
errToString err =
    case err of
        UsernameMissing ->
            "Please enter username"

        PasswordMissing ->
            "Please enter password"


loginValidator : Validator ( LoginField, LoginValidationError ) Model
loginValidator =
    Validate.all
        [ ifBlank .username ( Username, UsernameMissing )
        , ifBlank .password ( Password, PasswordMissing )
        ]


makeLoginRequestModel : Model -> Maybe LoginRequest
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
