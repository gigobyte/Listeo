module Pages.Register.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.Register.Api as Api
import Pages.Register.Model exposing (Model)
import Pages.Register.Validation exposing (makeRegisterRequestModel)
import RemoteData exposing (RemoteData(..))


init : Model
init =
    { username = ""
    , password = ""
    , showErrors = False
    , registerResponse = NotAsked
    }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        RegisterUsernameUpdated value ->
            ( { model | username = String.trim value }, Cmd.none )

        RegisterPasswordUpdated value ->
            ( { model | password = value }, Cmd.none )

        RegisterAttempted ->
            case makeRegisterRequestModel model of
                Just request ->
                    ( model, Api.register request |> Cmd.map Register )

                Nothing ->
                    ( { model | showErrors = True }, Cmd.none )

        Register response ->
            ( { model | registerResponse = response }, Cmd.none )

        _ ->
            ( model, Cmd.none )
