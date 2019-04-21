module Session exposing (Session, fromModel)

import Model exposing (AppModel)
import Msg exposing (Msg)
import Route exposing (Route)
import Utils.Fetch exposing (ApiRoot, Token)


type alias Session =
    { pushUrl : Route -> Cmd Msg
    , route : Route
    , apiRoot : ApiRoot
    , token : Token
    }


fromModel : AppModel -> Session
fromModel model =
    { route = model.route
    , pushUrl = \route -> Route.pushUrl model.key route
    , apiRoot = model.apiRoot
    , token = model.auth.jwt
    }
