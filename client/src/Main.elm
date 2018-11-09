module Main exposing (Model, main)

import Auth as Auth
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Styled exposing (toUnstyled)
import List
import Maybe.Extra as Maybe
import Msg exposing (Msg(..))
import Pages.Login as Login
import Pages.Register as Register
import Routes exposing (Route, pushUrl)
import Url
import Url.Parser exposing (parse)


type alias Model =
    { key : Nav.Key
    , url : Maybe Route
    , login : Login.Model
    , register : Register.Model
    , auth : Auth.Model
    }


type alias Flags =
    { jwt : Maybe String
    }


redirectIfUnauthenticated : Maybe String -> Nav.Key -> Cmd msg
redirectIfUnauthenticated jwtToken key =
    jwtToken
        |> Maybe.unwrap (pushUrl key Routes.Login) (always Cmd.none)


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init flags url key =
    ( { key = key
      , url = parse Routes.parser url
      , login = Login.init
      , register = Register.init
      , auth = Auth.init flags.jwt
      }
    , redirectIfUnauthenticated flags.jwt key
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = parse Routes.parser url }, Cmd.none )

        _ ->
            let
                ( newLoginModel, loginMsg ) =
                    Login.update model.login msg

                ( newRegisterModel, registerMsg ) =
                    Register.update model.register msg
            in
            Debug.log ""
                ( { model
                    | login = newLoginModel
                    , register = newRegisterModel
                  }
                , Cmd.batch
                    [ loginMsg
                    , registerMsg
                    ]
                )


view : Model -> Browser.Document Msg
view model =
    { title = "Hello world"
    , body =
        List.singleton <|
            case model.url of
                Just Routes.Login ->
                    model.login |> Login.view |> toUnstyled

                Just Routes.Register ->
                    model.register |> Register.view |> toUnstyled

                Just Routes.About ->
                    text "NotImplementedException"

                Just Routes.Home ->
                    text "NotImplementedException"

                Nothing ->
                    text "Not Found"
    }


subscriptions : Model -> Sub Msg
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
