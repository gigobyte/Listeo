module Main exposing (main)

import Auth.Api as Api
import Auth.Selectors as Selectors
import Auth.Token exposing (Token(..))
import Auth.Update as Auth
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Styled exposing (toUnstyled)
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Pages.Colors as Colors
import Pages.CreatePlaylist as CreatePlaylist
import Pages.Header as Header
import Pages.Home as Home
import Pages.Layout as Layout
import Pages.Login as Login
import Pages.Register as Register
import Route
import Url


type alias Flags =
    { jwt : Maybe String
    , apiRoot : String
    }


init : Flags -> Url.Url -> Nav.Key -> ( AppModel, Cmd Msg )
init flags url key =
    ( { key = key
      , route = Route.parseUrl url
      , apiRoot = flags.apiRoot
      , login = Login.init
      , register = Register.init
      , auth = Auth.init flags.jwt
      , home = Home.init
      , createPlaylist = CreatePlaylist.init
      , header = Header.init
      }
    , Api.fetchUser flags.apiRoot (Token flags.jwt) |> Cmd.map FetchUser
    )


mainUpdate : Msg -> AppModel -> ( AppModel, Cmd Msg )
mainUpdate msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Auth.pushAuthUrl
                        (\route -> Route.pushUrl model.key route)
                        (Route.parseUrl url)
                        (Selectors.getUser model)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Route.parseUrl url }, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Msg: " msg

        session =
            { route = model.route
            , pushUrl = \route -> Route.pushUrl model.key route
            , apiRoot = model.apiRoot
            }

        ( newMainModel, mainMsg ) =
            mainUpdate msg model

        ( newLoginModel, loginMsg ) =
            Login.update msg model session

        ( newRegisterModel, registerMsg ) =
            Register.update msg model session

        ( newAuthModel, authMsg ) =
            Auth.update msg model session

        ( newCreatePlaylistModel, createPlaylistMsg ) =
            CreatePlaylist.update msg model session

        ( newHeaderModel, headerMsg ) =
            Header.update msg model session
    in
    Debug.log ""
        ( { newMainModel
            | login = newLoginModel
            , register = newRegisterModel
            , auth = newAuthModel
            , createPlaylist = newCreatePlaylistModel
            , header = newHeaderModel
          }
        , Cmd.batch
            [ mainMsg
            , loginMsg
            , registerMsg
            , authMsg
            , createPlaylistMsg
            , headerMsg
            ]
        )


view : AppModel -> Browser.Document Msg
view model =
    { title =
        case model.route of
            Route.Login ->
                Login.title model

            Route.Register ->
                Register.title model

            Route.About ->
                "NotImplementedException"

            Route.Home ->
                Home.title model

            Route.CreatePlaylist ->
                CreatePlaylist.title model

            Route.NotFound404 ->
                "404 - Listeo"

            Route.DebugColors ->
                "Colors page - DEBUG ONLY"
    , body =
        [ case model.route of
            Route.Login ->
                Layout.view (Login.view model) model |> toUnstyled

            Route.Register ->
                Layout.view (Register.view model) model |> toUnstyled

            Route.About ->
                text "NotImplementedException"

            Route.Home ->
                Layout.view (Home.view model) model |> toUnstyled

            Route.CreatePlaylist ->
                Layout.view (CreatePlaylist.view model) model |> toUnstyled

            Route.NotFound404 ->
                text "Not Found"

            Route.DebugColors ->
                Layout.view Colors.view model |> toUnstyled
        ]
    }


subscriptions : AppModel -> Sub Msg
subscriptions =
    always Sub.none


main : Program Flags AppModel Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
