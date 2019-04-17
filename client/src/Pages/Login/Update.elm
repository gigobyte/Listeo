port module Pages.Login.Update exposing (init, update)

import Auth.Api as Api
import Msg exposing (Msg(..))
import Pages.Login.Api as Api exposing (LoginResponse)
import Pages.Login.Model exposing (Model)
import Pages.Login.Validation as Validation
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session)
import Utils.Fetch exposing (Token(..))


init : Model
init =
    { username = ""
    , password = ""
    , showErrors = False
    , loginResponse = NotAsked
    }


port storeJwt : String -> Cmd msg


update : Msg -> Model -> Session -> ( Model, Cmd Msg )
update msg model session =
    case msg of
        LoginUsernameUpdated value ->
            ( { model | username = String.trim value }, Cmd.none )

        LoginPasswordUpdated value ->
            ( { model | password = value }, Cmd.none )

        LoginAttempted ->
            case Validation.makeLoginRequestModel model of
                Just request ->
                    ( model, Api.login session.apiRoot request |> Cmd.map Login )

                Nothing ->
                    ( { model | showErrors = True }, Cmd.none )

        Login ((Success { jwt }) as response) ->
            ( { model | loginResponse = response }
            , Cmd.batch
                [ Api.fetchUser session.apiRoot (Token (Just jwt)) |> Cmd.map FetchUser
                , session.pushUrl Route.Home
                , storeJwt jwt
                ]
            )

        Login response ->
            ( { model | loginResponse = response }, Cmd.none )

        UrlChanged _ ->
            ( if session.route == Route.Login then
                init

              else
                model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
