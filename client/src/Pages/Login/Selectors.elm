module Pages.Login.Selectors exposing
    ( getLoginRequestErrorText
    , getValidationErrors
    )

import Pages.Login.Api as Api exposing (LoginResponse(..))
import Pages.Login.Model exposing (Model)
import Pages.Login.Validation exposing (LoginField, LoginValidationError, loginValidator)
import RemoteData exposing (RemoteData(..))
import Result.Extra as Result
import Validate exposing (validate)


getValidationErrors : Model -> List ( LoginField, LoginValidationError )
getValidationErrors model =
    if model.showErrors then
        validate loginValidator model
            |> Result.map (always [])
            |> Result.merge

    else
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
