module Pages.Login.Validation exposing (LoginValidationError, loginValidator)

import Pages.Login.Model exposing (Model)
import Validate exposing (Validator, ifBlank)


type LoginValidationError
    = UsernameMissing
    | PasswordMissing


loginValidator : Validator LoginValidationError Model
loginValidator =
    Validate.firstError
        [ ifBlank .username UsernameMissing
        , ifBlank .password PasswordMissing
        ]
