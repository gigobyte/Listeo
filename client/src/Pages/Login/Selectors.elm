module Pages.Login.Selectors exposing
    ( getLoginRequestErrorText
    , getPasswordError
    , getPasswordValue
    , getUsernameError
    , getUsernameValue
    , getValidationErrors
    , isSubmitButtonDisabled
    )

import Model exposing (AppModel)
import Pages.Login.Api as Api exposing (LoginResponse(..), LoginResponseError(..))
import Pages.Login.Validation as Validation exposing (LoginField(..), LoginValidationError(..), loginValidator)
import RemoteData exposing (RemoteData(..), isLoading)
import Result.Extra as Result
import Utils.Validation exposing (getErrorForField)
import Validate exposing (validate)


loginErrorToString : LoginResponseError -> String
loginErrorToString err =
    case err of
        UserNotFound ->
            "User not found"

        ServerError ->
            "Something went wrong"

        ValidationFailed ->
            ""


getValidationErrors : AppModel -> List ( LoginField, LoginValidationError )
getValidationErrors model =
    if model.login.showErrors then
        validate loginValidator model.login
            |> Result.map (always [])
            |> Result.merge

    else
        []


getLoginRequestErrorText : AppModel -> String
getLoginRequestErrorText model =
    case model.login.loginResponse of
        Success res ->
            case res of
                ErrorResponse { errorDescription } ->
                    loginErrorToString errorDescription

                _ ->
                    ""

        _ ->
            ""


isSubmitButtonDisabled : AppModel -> Bool
isSubmitButtonDisabled model =
    isLoading model.login.loginResponse
        || (List.length (getValidationErrors model) > 0)


getPasswordError : AppModel -> Maybe String
getPasswordError model =
    getErrorForField Password (getValidationErrors model)
        |> Maybe.map Validation.errToString


getPasswordValue : AppModel -> String
getPasswordValue model =
    model.login.password


getUsernameError : AppModel -> Maybe String
getUsernameError model =
    getErrorForField Username (getValidationErrors model)
        |> Maybe.map Validation.errToString


getUsernameValue : AppModel -> String
getUsernameValue model =
    model.login.username
