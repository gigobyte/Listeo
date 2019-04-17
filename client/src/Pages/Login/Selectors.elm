module Pages.Login.Selectors exposing
    ( getLoginRequestErrorText
    , getPasswordError
    , getPasswordValue
    , getUsernameError
    , getUsernameValue
    , getValidationErrors
    , isSubmitButtonDisabled
    )

import Pages.Login.Api as Api exposing (LoginResponse, LoginResponseError(..))
import Pages.Login.Model as Login
import Pages.Login.Validation as Validation exposing (LoginField(..), LoginValidationError(..), loginValidator)
import RemoteData exposing (RemoteData(..), isLoading)
import Result.Extra as Result
import Utils.ErrorResponse exposing (HttpError(..))
import Utils.Validation exposing (getErrorForField)
import Validate exposing (validate)


loginErrorToString : LoginResponseError -> String
loginErrorToString err =
    case err of
        UserNotFound ->
            "User not found"

        _ ->
            "Something went wrong"


getValidationErrors : Login.Model -> List ( LoginField, LoginValidationError )
getValidationErrors model =
    if model.showErrors then
        validate loginValidator model
            |> Result.map (always [])
            |> Result.merge

    else
        []


getLoginRequestErrorText : Login.Model -> String
getLoginRequestErrorText model =
    case model.loginResponse of
        Failure (BadResponse error) ->
            loginErrorToString error

        _ ->
            ""


isSubmitButtonDisabled : Login.Model -> Bool
isSubmitButtonDisabled model =
    isLoading model.loginResponse
        || (List.length (getValidationErrors model) > 0)


getPasswordError : Login.Model -> Maybe String
getPasswordError model =
    getErrorForField Password (getValidationErrors model)
        |> Maybe.map Validation.errToString


getPasswordValue : Login.Model -> String
getPasswordValue model =
    model.password


getUsernameError : Login.Model -> Maybe String
getUsernameError model =
    getErrorForField Username (getValidationErrors model)
        |> Maybe.map Validation.errToString


getUsernameValue : Login.Model -> String
getUsernameValue model =
    model.username
