module Main exposing (Model, main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Styled exposing (toUnstyled)
import List
import Maybe.Extra as Maybe
import Msg exposing (Msg(..))
import Pages.Login as Login
import Pages.Login.Model as Login
import Routes exposing (pushUrl)
import Url
import Url.Parser exposing (parse)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , login : Login.Model
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
      }
    , redirectIfUnauthenticated flags.jwt key
    )


combinedUpdate : Model -> Msg -> ( Model, Cmd Msg )
combinedUpdate =
    \model msg ->
        let
            ( newModel, desiredMsg ) =
                Login.update model.login msg
        in
        ( { model | login = newModel }, desiredMsg )


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
            ( { model | url = url }, Cmd.none )

        _ ->
            combinedUpdate model msg


view : Model -> Browser.Document Msg
view model =
    { title = "Hello world"
    , body =
        List.singleton <|
            case parse Routes.parser model.url of
                Just Routes.Login ->
                    model.login |> Login.view |> toUnstyled

                Nothing ->
                    text "Not Found"

                _ ->
                    text ""
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
