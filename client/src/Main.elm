port module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global exposing (Snippet, global, typeSelector)
import Html.Styled as Html exposing (main_, styled, text)
import Pages.Colors as Colors
import Pages.CreatePlaylist as CreatePlaylist
import Pages.Header as Header
import Pages.Header.AddPlaylistModal as AddPlaylistModal
import Pages.Home as Home
import Pages.Login as Login
import Pages.Register as Register
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Session exposing (Msg(..), Session, User, fetchUser, jwtStored)
import UI.Colors exposing (gray200)
import Url exposing (Url)
import Utils.ErrorResponse exposing (ResponseData)
import Utils.Fetch exposing (ApiRoot(..), Token(..))
import Utils.Styles exposing (StyledDocument, StyledElement, addIfNeeded, toUnstyledDocument)


type Msg
    = NoOp
    | LinkClicked UrlRequest
    | UrlChanged Url
    | GotLoginMsg Login.Msg
    | GotHomeMsg Home.Msg
    | GotRegisterMsg Register.Msg
    | GotCreatePlaylistMsg CreatePlaylist.Msg
    | GotSessionMsg Session.Msg


type Model
    = Redirect Session
    | Login Login.Model
    | Register Register.Model
    | Home Home.Model
    | CreatePlaylist CreatePlaylist.Model
    | NotFound Session
    | DebugColors Session
    | About Session


type alias Flags =
    { jwt : Token
    , apiRoot : ApiRoot
    }


type alias RawFlags =
    { jwt : Maybe String
    , apiRoot : String
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.parseUrl url

        ( model, cmd ) =
            changeRouteTo route
                (Redirect
                    (Session.init
                        { navKey = key
                        , route = route
                        , apiRoot = flags.apiRoot
                        , token = flags.jwt
                        }
                    )
                )
    in
    ( model
    , Cmd.batch
        [ cmd
        , fetchUser flags.apiRoot flags.jwt |> Cmd.map (GotSessionMsg << FetchUser)
        ]
    )


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        Route.NotFound404 ->
            ( NotFound session, Cmd.none )

        Route.DebugColors ->
            ( DebugColors session, Cmd.none )

        Route.About ->
            ( About session, Cmd.none )

        Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg

        Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg

        Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg

        Route.CreatePlaylist ->
            CreatePlaylist.init session
                |> updateWith CreatePlaylist GotCreatePlaylistMsg

        Route.ViewPlaylist _ ->
            ( NotFound session, Cmd.none )


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        DebugColors session ->
            session

        About session ->
            session

        Login login ->
            Login.toSession login

        Register register ->
            Register.toSession register

        Home home ->
            Home.toSession home

        CreatePlaylist createPlaylist ->
            CreatePlaylist.toSession createPlaylist


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Msg: " msg
    in
    Debug.log ""
        (case ( msg, model ) of
            ( NoOp, _ ) ->
                ( model, Cmd.none )

            ( LinkClicked urlRequest, _ ) ->
                case urlRequest of
                    Browser.Internal url ->
                        ( model
                        , Session.pushAuthUrl
                            (toSession model).navKey
                            (Route.parseUrl url)
                            (Session.getUser (toSession model))
                        )

                    Browser.External href ->
                        ( model, Nav.load href )

            ( UrlChanged url, _ ) ->
                changeRouteTo (Route.parseUrl url) model

            ( GotSessionMsg subMsg, _ ) ->
                updateSession subMsg model

            ( GotLoginMsg subMsg, Login login ) ->
                Login.update subMsg login
                    |> updateWith Login GotLoginMsg

            ( GotRegisterMsg subMsg, Register register ) ->
                Register.update subMsg register
                    |> updateWith Register GotRegisterMsg

            ( GotHomeMsg subMsg, Home home ) ->
                Home.update subMsg home
                    |> updateWith Home GotHomeMsg

            ( GotCreatePlaylistMsg subMsg, CreatePlaylist createPlaylist ) ->
                CreatePlaylist.update subMsg createPlaylist
                    |> updateWith CreatePlaylist GotCreatePlaylistMsg

            ( _, _ ) ->
                Debug.log ("Stray Message: " ++ Debug.toString msg) ( model, Cmd.none )
        )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


updateSession : Session.Msg -> Model -> ( Model, Cmd Msg )
updateSession subMsg page =
    let
        ( newSession, sessionMsg ) =
            Session.update subMsg (toSession page)

        newModel =
            case page of
                Redirect _ ->
                    Redirect newSession

                NotFound _ ->
                    NotFound newSession

                About _ ->
                    About newSession

                DebugColors _ ->
                    DebugColors newSession

                Login login ->
                    Login (Login.updateSession newSession login)

                Register register ->
                    Register (Register.updateSession newSession register)

                Home home ->
                    Home (Home.updateSession newSession home)

                CreatePlaylist createPlaylist ->
                    CreatePlaylist (CreatePlaylist.updateSession newSession createPlaylist)
    in
    ( newModel, Cmd.map GotSessionMsg sessionMsg )


viewContainer : StyledElement msg
viewContainer =
    styled main_
        [ height <| pct 100
        , flexDirection column
        , displayFlex
        ]


globalStyle : List Snippet
globalStyle =
    [ typeSelector "html, body"
        [ height <| pct 100
        , margin zero
        , fontFamilies [ "Museo-Sans" ]
        , backgroundColor gray200
        ]
    ]


viewNothing : StyledDocument msg
viewNothing =
    { title = "", body = [ text "" ] }


viewLayout : StyledDocument Msg -> Model -> StyledDocument Msg
viewLayout page model =
    let
        session =
            toSession model
    in
    case session.user of
        NotAsked ->
            viewNothing

        Loading ->
            viewNothing

        _ ->
            { title = page.title
            , body =
                [ viewContainer []
                    (List.concat
                        [ [ global globalStyle
                          , Header.view session |> Html.map GotSessionMsg
                          ]
                        , addIfNeeded session.header.isOverlayShown [ AddPlaylistModal.view |> Html.map GotSessionMsg ]
                        , page.body
                        ]
                    )
                ]
            }


view : Model -> Browser.Document Msg
view model =
    let
        viewPage { title, body } toMsg =
            viewLayout { title = title, body = List.map (Html.map toMsg) body } model
    in
    toUnstyledDocument <|
        case model of
            Redirect _ ->
                { title = "", body = [] }

            Login login ->
                viewPage (Login.view login) GotLoginMsg

            Register register ->
                viewPage (Register.view register) GotRegisterMsg

            NotFound _ ->
                { title = "404 - Listeo", body = [ text "Not Found" ] }

            About _ ->
                { title = "About - Listeo", body = [ text "About" ] }

            DebugColors _ ->
                viewPage Colors.view (always NoOp)

            Home home ->
                viewPage (Home.view home) GotHomeMsg

            CreatePlaylist createPlaylist ->
                viewPage (CreatePlaylist.view createPlaylist) GotCreatePlaylistMsg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ jwtStored (GotSessionMsg << JwtStored)
        ]


main : Program RawFlags Model Msg
main =
    Browser.application
        { init = \val -> init { jwt = Token val.jwt, apiRoot = ApiRoot val.apiRoot }
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
