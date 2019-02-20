port module Auth.Update exposing
    ( init
    , isAuthDisallowedRoute
    , isAuthProtectedRoute
    , pushAuthUrl
    , update
    )

import Auth.Api as Api exposing (User)
import Auth.Model exposing (Model)
import Env exposing (Env)
import Http
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Pages.Login.Api exposing (LoginResponse(..))
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)


port removeJwt : () -> Cmd msg


init : Maybe String -> Model
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


update : Msg -> AppModel -> Env -> ( Model, Cmd Msg )
update msg model env =
    updateAuth msg model.auth env


updateAuth : Msg -> Model -> Env -> ( Model, Cmd Msg )
updateAuth msg model { pushUrl, route } =
    case msg of
        Login (Success (SuccessResponse { jwt })) ->
            ( { model | jwt = Just jwt }, Cmd.none )

        Logout ->
            ( init Nothing
            , Cmd.batch
                [ removeJwt ()
                , Api.fetchUser "" |> Cmd.map FetchUser
                ]
            )

        FetchUser ((Success user) as response) ->
            ( { model | user = response }, pushAuthUrl pushUrl route (Just user) )

        FetchUser ((Failure (Http.BadStatus { status })) as response) ->
            ( { model | user = response }
            , if status.code == 401 then
                pushAuthUrl pushUrl route Nothing

              else
                Cmd.none
            )

        FetchUser response ->
            ( { model | user = response }, Cmd.none )

        _ ->
            ( model, Cmd.none )
