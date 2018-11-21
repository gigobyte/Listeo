module Auth.Update exposing (init, update)

import Auth.Model exposing (Model)
import Browser.Navigation as Nav
import Http
import Msg exposing (Msg(..))
import Pages.Login.Api exposing (LoginResponse(..))
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)


type alias Meta =
    { key : Nav.Key
    , url : Route
    }


init : Maybe String -> Model
init jwtFromFlag =
    { jwt = jwtFromFlag
    , user = Nothing
    }


isAuthProtectedRoute : Route -> Bool
isAuthProtectedRoute route =
    route == Route.Home


isAuthDisallowedRoute : Route -> Bool
isAuthDisallowedRoute route =
    route == Route.Login || route == Route.Register


fetchUserSuccessCmd : Route -> Nav.Key -> Cmd Msg
fetchUserSuccessCmd route key =
    if isAuthDisallowedRoute route then
        Route.pushUrl key Route.Home

    else
        Cmd.none


fetchUserFailureCmd : Route -> Nav.Key -> Cmd Msg
fetchUserFailureCmd route key =
    if isAuthProtectedRoute route then
        Route.pushUrl key Route.Login

    else
        Cmd.none


update : Msg -> Model -> Meta -> ( Model, Cmd Msg )
update msg model meta =
    case msg of
        Login (Success (SuccessResponse { jwt })) ->
            ( { model | jwt = Just jwt }, Cmd.none )

        FetchUser (Success user) ->
            ( { model | user = Just user }, fetchUserSuccessCmd meta.url meta.key )

        FetchUser (Failure (Http.BadStatus { status })) ->
            ( model
            , if status.code == 401 then
                fetchUserFailureCmd meta.url meta.key

              else
                Cmd.none
            )

        _ ->
            ( model, Cmd.none )
