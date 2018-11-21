module Auth.Update exposing (init, update)

import Auth.Model exposing (Model)
import Msg exposing (Msg(..))
import Pages.Login.Api exposing (LoginResponse(..))
import RemoteData exposing (RemoteData(..))


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

        FetchUser (Success user) ->
            ( { model | user = Just user }, Cmd.none )

        _ ->
            ( model, Cmd.none )
