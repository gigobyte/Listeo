module Main exposing (main)

import Auth.Api as Api
import Auth.Update as Auth
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Styled exposing (toUnstyled)
import List
import Maybe.Extra as Maybe
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Pages.CreatePlaylist as CreatePlaylist
import Pages.Header as Header
import Pages.Home as Home
import Pages.Layout as Layout
import Pages.Login as Login
import Pages.Register as Register
import Route exposing (Route)
import Url
import Url.Parser exposing (parse)


type alias Flags =
    { jwt : Maybe String
    }


fetchUser : Maybe String -> Cmd Msg
fetchUser token =
    Api.fetchUser (token |> Maybe.withDefault "") |> Cmd.map FetchUser


init : Flags -> Url.Url -> Nav.Key -> ( AppModel, Cmd Msg )
init flags url key =
    ( { key = key
      , url = Route.parseUrl url
      , login = Login.init
      , register = Register.init
      , auth = Auth.init flags.jwt
      , home = Home.init
      , createPlaylist = CreatePlaylist.init
      , header = Header.init
      }
    , fetchUser flags.jwt
    )


mainUpdate : Msg -> AppModel -> ( AppModel, Cmd Msg )
mainUpdate msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Auth.pushAuthUrl (Route.parseUrl url) model.key model.auth.user )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = Route.parseUrl url }, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Msg: " msg

        ( newMainModel, mainMsg ) =
            mainUpdate msg model

        ( newLoginModel, loginMsg ) =
            Login.update msg model

        ( newRegisterModel, registerMsg ) =
            Register.update msg model

        ( newAuthModel, authMsg ) =
            Auth.update msg model.auth { key = model.key, url = model.url }

        ( newCreatePlaylistModel, createPlaylistMsg ) =
            CreatePlaylist.update msg model.createPlaylist

        ( newHeaderModel, headerMsg ) =
            Header.update msg model.header { key = model.key }
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
        case model.url of
            Route.Login ->
                Login.title model

            Route.Register ->
                Register.title model

            Route.About ->
                "NotImplementedException"

            Route.Home ->
                Home.title model.home

            Route.CreatePlaylist ->
                CreatePlaylist.title model.createPlaylist

            Route.NotFound404 ->
                "404 - Listeo"
    , body =
        [ case model.url of
            Route.Login ->
                Layout.view (Login.view model) model |> toUnstyled

            Route.Register ->
                Layout.view (Register.view model) model |> toUnstyled

            Route.About ->
                text "NotImplementedException"

            Route.Home ->
                Layout.view (model.home |> Home.view) model |> toUnstyled

            Route.CreatePlaylist ->
                Layout.view (model.createPlaylist |> CreatePlaylist.view) model |> toUnstyled

            Route.NotFound404 ->
                text "Not Found"
        ]
    }


subscriptions : AppModel -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
