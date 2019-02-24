module Env exposing (Env)

import Msg exposing (Msg)
import Route exposing (Route)


type alias Env =
    { pushUrl : Route -> Cmd Msg
    , route : Route
    , apiRoot : String
    }
