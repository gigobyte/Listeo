module Pages.Register.Selectors exposing
    ( getPasswordError
    , getPasswordValue
    , getRegisterRequestErrorText
    , getUsernameError
    , getUsernameValue
    , getValidationErrors
    , isSubmitButtonDisabled
    )

import Pages.Register.Api as Api exposing (RegisterResponseError(..))
import Pages.Register.Validation as Validation exposing (RegisterField(..), RegisterValidationError(..), registerValidator)
import Pages.Register.Model as Register
import RemoteData exposing (RemoteData(..), isLoading)
import Result.Extra as Result
import Utils.Validation exposing (getErrorForField)
import Validate exposing (validate)


registerErrorToString : RegisterResponseError -> String
registerErrorToString err =
    case err of
        UserAlreadyExists ->
            "User already exists"

        ServerError ->
            "Something went wrong"

        ValidationFailed ->
            ""


getValidationErrors : Register.Model -> List ( RegisterField, RegisterValidationError )
getValidationErrors model =
    if model.showErrors then
        validate registerValidator model
            |> Result.map (always [])
            |> Result.merge

    else
        []


getRegisterRequestErrorText : Register.Model -> String
getRegisterRequestErrorText model =
    case model.registerResponse of
        Success { errorDescription } ->
            errorDescription
                |> Maybe.map registerErrorToString
                |> Maybe.withDefault ""

        _ ->
            ""


isSubmitButtonDisabled : Register.Model -> Bool
isSubmitButtonDisabled model =
    isLoading model.registerResponse
        || (List.length (getValidationErrors model) > 0)


getPasswordError : Register.Model -> Maybe String
getPasswordError model =
    getErrorForField Password (getValidationErrors model)
        |> Maybe.map Validation.errToString


getPasswordValue : Register.Model -> String
getPasswordValue model =
    model.password


getUsernameError : Register.Model -> Maybe String
getUsernameError model =
    getErrorForField Username (getValidationErrors model)
        |> Maybe.map Validation.errToString


getUsernameValue : Register.Model -> String
getUsernameValue model =
    model.username
