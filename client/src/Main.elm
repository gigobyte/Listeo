module Main exposing (Model, main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Styled exposing (toUnstyled)
import List
import Maybe.Extra as Maybe
import Msg exposing (Msg(..))
import Pages.Login as Login
import Pages.Register as Register
import Routes exposing (pushUrl)
import Url
import Url.Parser exposing (parse)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , login : Login.Model
    , register : Register.Model
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
      , url = url
      , login = Login.init
      , register = Register.init
      }
    , redirectIfUnauthenticated flags.jwt key
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log (Debug.toString model) ""
    in
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        _ ->
            let
                ( newLoginModel, loginMsg ) =
                    Login.update model.login msg

                ( newRegisterModel, registerMsg ) =
                    Register.update model.register msg
            in
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
            case parse Routes.parser model.url of
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
