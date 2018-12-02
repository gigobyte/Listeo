port module Pages.Login.Update exposing (init, update)

import Auth.Api as Auth
import Browser.Navigation as Nav
import Msg exposing (Msg(..))
import Pages.Login.Api as Api exposing (LoginResponse(..))
import Pages.Login.Model exposing (Model)
import Pages.Login.Validation as Validation
import RemoteData exposing (RemoteData(..))
import Route as Route


type alias Context =
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


update : Msg -> Model -> Context -> ( Model, Cmd Msg )
update msg model ctx =
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
                        [ Auth.fetchUser jwt |> Cmd.map FetchUser
                        , Route.pushUrl ctx.key Route.Home
                        , storeJwt jwt
                        ]
                    )

                _ ->
                    ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )
