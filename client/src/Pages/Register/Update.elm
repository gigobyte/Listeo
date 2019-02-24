module Pages.Register.Update exposing (init, update)

import Msg exposing (Msg(..))
import Pages.Login.Api as Api
import Pages.Register.Api as Api
import Pages.Register.Model exposing (Model)
import Pages.Register.Validation exposing (makeRegisterRequestModel)
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session)


init : Model
init =
    { username = ""
    , password = ""
    , showErrors = False
    , registerResponse = NotAsked
    }


update : Msg -> Model -> Session -> ( Model, Cmd Msg )
update msg model { pushUrl, route, apiRoot } =
    case msg of
        RegisterUsernameUpdated value ->
            ( { model | username = String.trim value }, Cmd.none )

        RegisterPasswordUpdated value ->
            ( { model | password = value }, Cmd.none )

        RegisterAttempted ->
            case makeRegisterRequestModel model of
                Just request ->
                    ( model, Api.register apiRoot request |> Cmd.map Register )

                Nothing ->
                    ( { model | showErrors = True }, Cmd.none )

        Register ((Success { errorDescription }) as response) ->
            let
                newModel =
                    { model | registerResponse = response }
            in
            case errorDescription of
                Just _ ->
                    ( newModel, Cmd.none )

                Nothing ->
                    ( newModel
                    , Cmd.batch
                        [ Api.login
                            apiRoot
                            { username = model.username
                            , password = model.password
                            }
                            |> Cmd.map Login
                        , pushUrl Route.Home
                        ]
                    )

        UrlChanged _ ->
            ( if route == Route.Login then
                init

              else
                model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
