module Main exposing (Model, main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import List
import Pages.Home as Home
import Pages.Login as Login
import Routes
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


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init flags url key =
    ( { key = key, url = url, home = Home.init, login = Login.init }
    , case flags.jwt of
        Just _ ->
            Cmd.none

        Nothing ->
            Nav.pushUrl key "login"
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
                    Html.map HomeMsg (Home.view model.home)

                Just Routes.Login ->
                    Html.map LoginMsg (Login.view model.login)

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
