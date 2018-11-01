module Pages.Register.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.Register.Api as Api
import Pages.Register.Model exposing (Model)


init : Model
init =
    { username = ""
    , password = ""
    , showErrors = False
    }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        RegisterUsernameUpdated value ->
            ( { model | username = value }, Cmd.none )

        RegisterPasswordUpdated value ->
            ( { model | password = value }, Cmd.none )

        RegisterAttempted ->
            case Api.makeRegisterRequestModel model of
                Just request ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | showErrors = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )
