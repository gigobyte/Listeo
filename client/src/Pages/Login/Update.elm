module Pages.Login.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.Login.Api as Api
import Pages.Login.Model exposing (Model)


init : Model
init =
    { username = ""
    , password = ""
    , showErrors = False
    }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        LoginUsernameUpdated value ->
            ( { model | username = value }, Cmd.none )

        LoginPasswordUpdated value ->
            ( { model | password = value }, Cmd.none )

        LoginAttempted ->
            case Api.makeLoginRequestModel model of
                Just request ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | showErrors = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )
