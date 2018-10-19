module Pages.Login.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.Login.Model exposing (Model)


init : Model
init =
    { username = ""
    , password = ""
    }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        LoginUsernameUpdated value ->
            ( { model | username = value }, Cmd.none )

        LoginPasswordUpdated value ->
            ( { model | password = value }, Cmd.none )

        LoginAttempted ->
            Debug.todo ""

        _ ->
            ( model, Cmd.none )
