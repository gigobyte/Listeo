module Auth exposing (Model, User, init, update)

import Msg exposing (Msg)


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
    ( model, Cmd.none )
