module Pages.Login.Validation exposing (LoginField(..), LoginValidationError(..), errToString, loginValidator)

import Pages.Login.Model exposing (Model)
import Validate exposing (Validator, ifBlank)


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
