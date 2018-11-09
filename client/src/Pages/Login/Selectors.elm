module Pages.Login.Selectors exposing
    ( getLoginRequestErrorText
    , getValidationErrors
    , isSubmitButtonDisabled
    )

import Pages.Login.Api as Api exposing (LoginResponse(..))
import Pages.Login.Model exposing (Model)
import Pages.Login.Validation exposing (LoginField, LoginValidationError, loginValidator)
import RemoteData exposing (RemoteData(..))
import Result.Extra as Result
import Utils.Api exposing (isLoading)
import Validate exposing (validate)


getValidationErrors : Model -> List ( LoginField, LoginValidationError )
getValidationErrors model =
    case model.showErrors of
        True ->
            validate loginValidator model
                |> Result.map (always [])
                |> Result.merge

        False ->
            []


getLoginRequestErrorText : Model -> String
getLoginRequestErrorText model =
    case model.loginResponse of
        Success res ->
            case res of
                ErrorResponse { errorDescription } ->
                    Api.loginErrorToString errorDescription

                _ ->
                    ""

        _ ->
            ""


isSubmitButtonDisabled : Model -> Bool
isSubmitButtonDisabled model =
    isLoading model.loginResponse
        || (List.length (getValidationErrors model) > 0)
