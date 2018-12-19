port module Auth.Update exposing
    ( init
    , isAuthDisallowedRoute
    , isAuthProtectedRoute
    , pushAuthUrl
    , update
    )

import Auth.Api as Api exposing (User)
import Auth.Model exposing (Model)
import Browser.Navigation as Nav
import Http
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Pages.Login.Api exposing (LoginResponse(..))
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Selectors


port removeJwt : () -> Cmd msg


type alias Context =
    { key : Nav.Key
    , route : Route
    }


init : Maybe String -> Model
init jwtFromFlag =
    { jwt = jwtFromFlag
    , user = Nothing
    , fetchUserResponse = NotAsked
    }


isAuthProtectedRoute : Route -> Bool
isAuthProtectedRoute route =
    route == Route.Home


isAuthDisallowedRoute : Route -> Bool
isAuthDisallowedRoute route =
    route == Route.Login || route == Route.Register


pushAuthUrl : Route -> Nav.Key -> Maybe User -> Cmd msg
pushAuthUrl route key user =
    case user of
        Just _ ->
            if isAuthDisallowedRoute route then
                Route.pushUrl key Route.Home

            else
                Route.pushUrl key route

        Nothing ->
            if isAuthProtectedRoute route then
                Route.pushUrl key Route.Login

            else
                Route.pushUrl key route


update : Msg -> AppModel -> ( Model, Cmd Msg )
update msg model =
    updateAuth msg
        model.auth
        { key = Selectors.getNavKey model
        , route = Selectors.getRoute model
        }


updateAuth : Msg -> Model -> Context -> ( Model, Cmd Msg )
updateAuth msg model ctx =
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
            ( { model | fetchUserResponse = response, user = Just user }, pushAuthUrl ctx.route ctx.key (Just user) )

        FetchUser ((Failure (Http.BadStatus { status })) as response) ->
            ( { model | fetchUserResponse = response }
            , if status.code == 401 then
                pushAuthUrl ctx.route ctx.key Nothing

              else
                Cmd.none
            )

        FetchUser response ->
            ( { model | fetchUserResponse = response }, Cmd.none )

        _ ->
            ( model, Cmd.none )
