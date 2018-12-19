port module Pages.Login.Update exposing (init, update)

import Auth.Api as Api
import Browser.Navigation as Nav
import Msg exposing (Msg(..))
import Pages.Login.Api as Api exposing (LoginResponse(..))
import Pages.Login.Model exposing (Model)
import Pages.Login.Validation as Validation
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)


type alias Context =
    { key : Nav.Key
    , route : Route
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

        Login ((Success (SuccessResponse { jwt })) as response) ->
            ( { model | loginResponse = response }
            , Cmd.batch
                [ Api.fetchUser jwt |> Cmd.map FetchUser
                , Route.pushUrl ctx.key Route.Home
                , storeJwt jwt
                ]
            )

        Login response ->
            ( { model | loginResponse = response }, Cmd.none )

        UrlChanged _ ->
            ( case ctx.route == Route.Login of
                True ->
                    init

                False ->
                    model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
