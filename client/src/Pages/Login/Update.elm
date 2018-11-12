port module Pages.Login.Update exposing (init, update)

import Browser.Navigation as Nav
import Msg exposing (Msg(..))
import Pages.Login.Api as Api exposing (LoginResponse(..))
import Pages.Login.Model exposing (Model)
import Pages.Login.Validation as Validation
import RemoteData exposing (RemoteData(..))
import Route as Route


type alias Meta =
    { key : Nav.Key
    }


init : Model
init =
    { username = ""
    , password = ""
    , showErrors = False
    , loginResponse = NotAsked
    }


port storeJwt : String -> Cmd msg


update : Msg -> Model -> Meta -> ( Model, Cmd Msg )
update msg model meta =
    case msg of
        LoginUsernameUpdated value ->
            ( { model | username = String.trim value }, Cmd.none )

        LoginPasswordUpdated value ->
            ( { model | password = value }, Cmd.none )

        LoginAttempted ->
            case Validation.makeLoginRequestModel model of
                Just request ->
                    ( model, Api.login request |> Cmd.map Login )

                Nothing ->
                    ( { model | showErrors = True }, Cmd.none )

        Login response ->
            let
                newModel =
                    { model | loginResponse = response }
            in
            case response of
                Success (SuccessResponse { jwt }) ->
                    ( newModel
                    , Cmd.batch
                        [ Route.pushUrl meta.key Route.Home
                        , storeJwt jwt
                        ]
                    )

                _ ->
                    ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )
