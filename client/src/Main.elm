module Main exposing (main)

import Auth.Api as Api
import Auth.Selectors as Selector
import Auth.Update as Auth
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
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
import Utils.Fetch exposing (ApiRoot(..), Token(..))
import Utils.Styles exposing (toUnstyledDocument)


type alias Flags =
    { jwt : Token
    , apiRoot : ApiRoot
    }


type alias RawFlags =
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
    , Api.fetchUser flags.apiRoot flags.jwt |> Cmd.map FetchUser
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
                        (Selector.getUser model.auth)
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
            , token = model.auth.jwt
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
    case model.route of
        Route.Login ->
            Layout.view (Login.view model) model |> toUnstyledDocument

        Route.Register ->
            Layout.view (Register.view model) model |> toUnstyledDocument

        Route.About ->
            { title = "", body = [ text "NotImplementedException" ] }

        Route.Home ->
            Layout.view (Home.view model) model |> toUnstyledDocument

        Route.CreatePlaylist ->
            Layout.view (CreatePlaylist.view model) model |> toUnstyledDocument

        Route.NotFound404 ->
            { title = "404 - Listeo", body = [ text "Not Found" ] }

        Route.DebugColors ->
            Layout.view Colors.view model |> toUnstyledDocument


subscriptions : AppModel -> Sub Msg
subscriptions =
    always Sub.none


main : Program RawFlags AppModel Msg
main =
    Browser.application
        { init = \val -> init { jwt = Token val.jwt, apiRoot = ApiRoot val.apiRoot }
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
