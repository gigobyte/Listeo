module Pages.Register.Update exposing (init, update)

import Browser.Navigation as Nav
import Msg exposing (Msg(..))
import Pages.Login.Api as Api
import Pages.Register.Api as Api
import Pages.Register.Model exposing (Model)
import Pages.Register.Validation exposing (makeRegisterRequestModel)
import RemoteData exposing (RemoteData(..))
import Routes as Route


type alias Meta =
    { key : Nav.Key
    }


init : Model
init =
    { username = ""
    , password = ""
    , showErrors = False
    , registerResponse = NotAsked
    }


update : Msg -> Model -> Meta -> ( Model, Cmd Msg )
update msg model meta =
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
            let
                newModel =
                    { model | registerResponse = response }
            in
            case response of
                Success { errorDescription } ->
                    case errorDescription of
                        Just _ ->
                            ( newModel, Cmd.none )

                        Nothing ->
                            ( newModel
                            , Cmd.batch
                                [ Api.login
                                    { username = model.username
                                    , password = model.password
                                    }
                                    |> Cmd.map Login
                                , Route.pushUrl meta.key Route.Home
                                ]
                            )

                _ ->
                    ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )
