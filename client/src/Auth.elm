module Auth exposing (Model, User, init)


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
