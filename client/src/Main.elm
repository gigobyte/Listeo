module Main exposing (main)

import Auth as Auth
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Styled exposing (toUnstyled)
import List
import Maybe.Extra as Maybe
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Pages.Home as Home
import Pages.Layout as Layout
import Pages.Login as Login
import Pages.Register as Register
import Route exposing (Route, pushUrl)
import Url
import Url.Parser exposing (parse)


type alias Flags =
    { jwt : Maybe String
    }


redirectIfUnauthenticated : Maybe String -> Nav.Key -> Cmd msg
redirectIfUnauthenticated jwtToken key =
    jwtToken
        |> Maybe.unwrap (pushUrl key Route.Login) (always Cmd.none)


init : Flags -> Url.Url -> Nav.Key -> ( AppModel, Cmd msg )
init flags url key =
    ( { key = key
      , url = parse Route.parser url
      , login = Login.init
      , register = Register.init
      , auth = Auth.init flags.jwt
      , home = Home.init
      }
    , redirectIfUnauthenticated flags.jwt key
    )


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = parse Route.parser url }, Cmd.none )

        _ ->
            let
                ( newLoginModel, loginMsg ) =
                    Login.update msg model.login { key = model.key }

                ( newRegisterModel, registerMsg ) =
                    Register.update msg model.register { key = model.key }

                ( newAuthModel, authMsg ) =
                    Auth.update msg model.auth
            in
            Debug.log ""
                ( { model
                    | login = newLoginModel
                    , register = newRegisterModel
                    , auth = newAuthModel
                  }
                , Cmd.batch
                    [ loginMsg
                    , registerMsg
                    , authMsg
                    ]
                )


view : AppModel -> Browser.Document Msg
view model =
    { title = "Hello world"
    , body =
        List.singleton <|
            case model.url of
                Just Route.Login ->
                    Layout.view (model.login |> Login.view) model |> toUnstyled

                Just Route.Register ->
                    Layout.view (model.register |> Register.view) model |> toUnstyled

                Just Route.About ->
                    text "NotImplementedException"

                Just Route.Home ->
                    Layout.view (model.home |> Home.view) model |> toUnstyled

                Nothing ->
                    text "Not Found"
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
