module Pages.Register.Selectors exposing
    ( getRegisterRequestErrorText
    , getValidationErrors
    , isSubmitButtonDisabled
    )

import Pages.Register.Api as Api exposing (RegisterResponse)
import Pages.Register.Model exposing (Model)
import Pages.Register.Validation exposing (RegisterField, RegisterValidationError, registerValidator)
import RemoteData exposing (RemoteData(..))
import Result.Extra as Result
import Utils.Api exposing (isLoading)
import Validate exposing (validate)


getValidationErrors : Model -> List ( RegisterField, RegisterValidationError )
getValidationErrors model =
    case model.showErrors of
        True ->
            validate registerValidator model
                |> Result.map (always [])
                |> Result.merge

        False ->
            []


getRegisterRequestErrorText : Model -> String
getRegisterRequestErrorText model =
    case model.registerResponse of
        Success { errorDescription } ->
            errorDescription
                |> Maybe.map Api.registerErrorToString
                |> Maybe.withDefault ""

        _ ->
            ""


isSubmitButtonDisabled : Model -> Bool
isSubmitButtonDisabled model =
    isLoading model.registerResponse
        || (List.length (getValidationErrors model) > 0)
