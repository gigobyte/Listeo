module Pages.Register.Validation exposing (RegisterField(..), RegisterValidationError(..), errToString, registerValidator)

import Pages.Register.Model exposing (Model)
import Validate exposing (Validator, ifBlank, ifTrue)


type RegisterValidationError
    = UsernameMissing
    | PasswordMissing
    | UsernameTooShort
    | PasswordTooShort


type RegisterField
    = Username
    | Password


usernameMinimumLength =
    4


passwordMinimumLength =
    6


errToString : RegisterValidationError -> String
errToString err =
    case err of
        UsernameMissing ->
            "Please enter username"

        PasswordMissing ->
            "Please enter password"

        UsernameTooShort ->
            "Username must be at least " ++ String.fromInt usernameMinimumLength ++ " characters long"

        PasswordTooShort ->
            "Password must be at least " ++ String.fromInt passwordMinimumLength ++ " characters long"


registerValidator : Validator ( RegisterField, RegisterValidationError ) Model
registerValidator =
    Validate.all
        [ ifBlank .username ( Username, UsernameMissing )
        , ifBlank .password ( Password, PasswordMissing )
        , ifTrue (\form -> String.length form.username < usernameMinimumLength) ( Username, UsernameTooShort )
        , ifTrue (\form -> String.length form.password < passwordMinimumLength) ( Password, PasswordTooShort )
        ]
