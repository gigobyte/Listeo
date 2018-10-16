module Main exposing (Model, Msg, main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Styled exposing (toUnstyled)
import List
import Pages.Home as Home
import Pages.Login as Login
import Routes exposing (pushUrl)
import Url
import Url.Parser exposing (parse)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , home : Home.Model
    , login : Login.Model
    }


type alias Flags =
    { jwt : Maybe String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg


redirectIfUnauthenticated : Maybe String -> Nav.Key -> Cmd msg
redirectIfUnauthenticated jwtToken key =
    case jwtToken of
        Just _ ->
            Cmd.none

        Nothing ->
            pushUrl key Routes.Login


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init flags url key =
    ( { key = key, url = url, home = Home.init, login = Login.init }
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
            ( { model | url = url }, Cmd.none )

        HomeMsg homeMsg ->
            ( { model | home = Home.update model.home homeMsg }, Cmd.none )

        LoginMsg loginMsg ->
            ( { model | login = Login.update model.login loginMsg }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Hello world"
    , body =
        List.singleton <|
            case parse Routes.parser model.url of
                Just Routes.Home ->
                    Html.map HomeMsg (model.home |> Home.view)

                Just Routes.Login ->
                    Html.map LoginMsg (model.login |> Login.view |> toUnstyled)

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
