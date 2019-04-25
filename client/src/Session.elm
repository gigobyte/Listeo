port module Session exposing (Msg(..), Session, User, fetchUser, getUser, init, pushAuthUrl, update)

import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Utils.ErrorResponse exposing (HttpError(..), ResponseData, expectJsonWithError)
import Utils.Fetch as Fetch exposing (ApiRoot, Token, emptyToken, unwrapToken)


port removeJwt : () -> Cmd msg


port storeJwt : String -> Cmd msg


type Msg
    = FetchUser (ResponseData () User)
    | StoreToken Token
    | Logout
    | AddPlaylistOverlayShown
    | CreateNewPlaylistSelected
    | AddPlaylistModalClosed


type alias User =
    { username : String
    , createdOn : String
    }


getUser : Session -> Maybe User
getUser session =
    case session.user of
        Success user ->
            Just user

        _ ->
            Nothing


type alias Session =
    { navKey : Nav.Key
    , route : Route
    , apiRoot : ApiRoot
    , token : Token
    , user : ResponseData () User
    , header : HeaderModel
    }


type alias HeaderModel =
    { isOverlayShown : Bool
    }



-- UPDATE


isAuthProtectedRoute : Route -> Bool
isAuthProtectedRoute route =
    route == Route.Home


isAuthDisallowedRoute : Route -> Bool
isAuthDisallowedRoute route =
    route == Route.Login || route == Route.Register


pushAuthUrl : Nav.Key -> Route -> Maybe User -> Cmd msg
pushAuthUrl key route user =
    case user of
        Just _ ->
            if isAuthDisallowedRoute route then
                Route.pushUrl key Route.Home

            else
                Route.pushUrl key route

        Nothing ->
            if isAuthProtectedRoute route then
                Route.pushUrl key Route.Login

            else
                Route.pushUrl key route


init : { navKey : Nav.Key, route : Route, apiRoot : ApiRoot, token : Token } -> Session
init settings =
    { navKey = settings.navKey
    , route = settings.route
    , apiRoot = settings.apiRoot
    , token = settings.token
    , user = NotAsked
    , header = initHeader
    }


initHeader : HeaderModel
initHeader =
    { isOverlayShown = False
    }


reset : Session -> Session
reset session =
    { session
        | user = NotAsked
        , token = emptyToken
    }


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        StoreToken token ->
            ( { session | token = token }, storeJwt (unwrapToken token) )

        Logout ->
            ( reset session
            , Cmd.batch
                [ removeJwt ()
                , fetchUser session.apiRoot emptyToken |> Cmd.map FetchUser
                ]
            )

        FetchUser ((Success user) as response) ->
            ( { session | user = response }, pushAuthUrl session.navKey session.route (Just user) )

        FetchUser ((Failure Unauthorized) as response) ->
            ( { session | user = response }
            , pushAuthUrl session.navKey session.route Nothing
            )

        FetchUser response ->
            ( { session | user = response }, Cmd.none )

        AddPlaylistOverlayShown ->
            updateHeader (\header -> ( { header | isOverlayShown = True }, Cmd.none )) session

        CreateNewPlaylistSelected ->
            updateHeader (\header -> ( { header | isOverlayShown = False }, Route.pushUrl session.navKey {- Route.CreatePlaylist -} Route.Home )) session

        AddPlaylistModalClosed ->
            updateHeader (\header -> ( { header | isOverlayShown = False }, Cmd.none )) session


updateHeader : (HeaderModel -> ( HeaderModel, Cmd Msg )) -> Session -> ( Session, Cmd Msg )
updateHeader transform model =
    let
        ( newHeaderModel, cmd ) =
            transform model.header
    in
    ( { model | header = newHeaderModel }, cmd )



-- API


fetchUser : ApiRoot -> Token -> Cmd (ResponseData () User)
fetchUser apiRoot token =
    Fetch.getWithAuth
        { url = Fetch.currentUser apiRoot
        , token = token
        , expect = expectJsonWithError (Decode.succeed ()) userDecoder
        }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "username" string
        |> required "createdOn" string
