module Pages.Login.Validation exposing (LoginField(..), LoginValidationError(..), errToString, loginValidator)

import Pages.Login.Model exposing (Model)
import Validate exposing (Validator, ifBlank, ifTrue)


type LoginValidationError
    = UsernameMissing
    | PasswordMissing
    | UsernameTooShort
    | PasswordTooShort


type LoginField
    = Username
    | Password


usernameMinimumLength : Int
usernameMinimumLength =
    4


passwordMinimumLength : Int
passwordMinimumLength =
    6


errToString : LoginValidationError -> String
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


loginValidator : Validator ( LoginField, LoginValidationError ) Model
loginValidator =
    Validate.all
        [ ifBlank .username ( Username, UsernameMissing )
        , ifBlank .password ( Password, PasswordMissing )
        , ifTrue (\form -> String.length form.username < usernameMinimumLength) ( Username, UsernameTooShort )
        , ifTrue (\form -> String.length form.password < passwordMinimumLength) ( Password, PasswordTooShort )
        ]
