module Auth exposing (Model, User, init, update)

import Msg exposing (Msg(..))
import Pages.Login.Api exposing (LoginResponse(..))
import RemoteData exposing (RemoteData(..))


type alias User =
    { username : String
    , createdOn : Int
    }


type alias Model =
    { jwt : Maybe String
    , user : Maybe User
    }


init : Maybe String -> Model
init jwtFromFlag =
    { jwt = jwtFromFlag
    , user = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login (Success (SuccessResponse { jwt })) ->
            ( { model | jwt = Just jwt }, Cmd.none )

        _ ->
            ( model, Cmd.none )
