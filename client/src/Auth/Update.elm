port module Auth.Update exposing
    ( init
    , isAuthDisallowedRoute
    , isAuthProtectedRoute
    , pushAuthUrl
    , update
    )

import Auth.Api as Api exposing (User)
import Auth.Model exposing (Model)
import Http
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Pages.Login.Api exposing (LoginResponse(..))
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Session exposing (Session)
import Utils.Fetch exposing (Token(..), emptyToken)


port removeJwt : () -> Cmd msg


init : Token -> Model
init jwtFromFlag =
    { jwt = jwtFromFlag
    , user = NotAsked
    }


isAuthProtectedRoute : Route -> Bool
isAuthProtectedRoute route =
    route == Route.Home


isAuthDisallowedRoute : Route -> Bool
isAuthDisallowedRoute route =
    route == Route.Login || route == Route.Register


pushAuthUrl : (Route -> Cmd msg) -> Route -> Maybe User -> Cmd msg
pushAuthUrl pushUrl route user =
    case user of
        Just _ ->
            if isAuthDisallowedRoute route then
                pushUrl Route.Home

            else
                pushUrl route

        Nothing ->
            if isAuthProtectedRoute route then
                pushUrl Route.Login

            else
                pushUrl route


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    updateAuth msg model.auth session


updateAuth : Msg -> Model -> Session -> ( Model, Cmd Msg )
updateAuth msg model { pushUrl, route, apiRoot } =
    case msg of
        Login (Success (SuccessResponse { jwt })) ->
            ( { model | jwt = Token (Just jwt) }, Cmd.none )

        Logout ->
            ( init emptyToken
            , Cmd.batch
                [ removeJwt ()
                , Api.fetchUser apiRoot emptyToken |> Cmd.map FetchUser
                ]
            )

        FetchUser ((Success user) as response) ->
            ( { model | user = response }, pushAuthUrl pushUrl route (Just user) )

        FetchUser ((Failure (Http.BadStatus status)) as response) ->
            ( { model | user = response }
            , if status == 401 then
                pushAuthUrl pushUrl route Nothing

              else
                Cmd.none
            )

        FetchUser response ->
            ( { model | user = response }, Cmd.none )

        _ ->
            ( model, Cmd.none )
