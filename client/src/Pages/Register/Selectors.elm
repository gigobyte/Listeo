module Pages.Register.Selectors exposing
    ( getPasswordError
    , getPasswordValue
    , getRegisterRequestErrorText
    , getUsernameError
    , getUsernameValue
    , getValidationErrors
    , isSubmitButtonDisabled
    )

import Model exposing (AppModel)
import Pages.Register.Api as Api exposing (RegisterResponseError(..))
import Pages.Register.Validation as Validation exposing (RegisterField(..), RegisterValidationError(..), registerValidator)
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


getValidationErrors : AppModel -> List ( RegisterField, RegisterValidationError )
getValidationErrors model =
    case model.register.showErrors of
        True ->
            validate registerValidator model.register
                |> Result.map (always [])
                |> Result.merge

        False ->
            []


getRegisterRequestErrorText : AppModel -> String
getRegisterRequestErrorText model =
    case model.register.registerResponse of
        Success { errorDescription } ->
            errorDescription
                |> Maybe.map registerErrorToString
                |> Maybe.withDefault ""

        _ ->
            ""


isSubmitButtonDisabled : AppModel -> Bool
isSubmitButtonDisabled model =
    isLoading model.register.registerResponse
        || (List.length (getValidationErrors model) > 0)


getPasswordError : AppModel -> Maybe String
getPasswordError model =
    getErrorForField Password (getValidationErrors model)
        |> Maybe.map Validation.errToString


getPasswordValue : AppModel -> String
getPasswordValue model =
    model.register.password


getUsernameError : AppModel -> Maybe String
getUsernameError model =
    getErrorForField Username (getValidationErrors model)
        |> Maybe.map Validation.errToString


getUsernameValue : AppModel -> String
getUsernameValue model =
    model.register.username
