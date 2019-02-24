module Session exposing (Session)

import Msg exposing (Msg)
import Route exposing (Route)


type alias Session =
    { pushUrl : Route -> Cmd Msg
    , route : Route
    , apiRoot : String
    }
