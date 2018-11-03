module Pages.Login.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.Login.Api as Api
import Pages.Login.Model exposing (Model)
import Pages.Login.Validation exposing (makeLoginRequestModel)
import RemoteData exposing (RemoteData(..))


init : Model
init =
    { username = ""
    , password = ""
    , showErrors = False
    , loginResponse = NotAsked
    }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        LoginUsernameUpdated value ->
            ( { model | username = String.trim value }, Cmd.none )

        LoginPasswordUpdated value ->
            ( { model | password = value }, Cmd.none )

        LoginAttempted ->
            case makeLoginRequestModel model of
                Just request ->
                    ( model, Api.login request |> Cmd.map Login )

                Nothing ->
                    ( { model | showErrors = True }, Cmd.none )

        Login response ->
            ( { model | loginResponse = response }, Cmd.none )

        _ ->
            ( model, Cmd.none )
