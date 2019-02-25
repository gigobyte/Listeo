module Session exposing (Session)

import Msg exposing (Msg)
import Route exposing (Route)
import Utils.Fetch exposing (ApiRoot)


type alias Session =
    { pushUrl : Route -> Cmd Msg
    , route : Route
    , apiRoot : ApiRoot
    }
