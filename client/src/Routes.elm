module Routes exposing (Route(..), parser, pushUrl)

import Browser.Navigation as Nav
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | Login


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Login ->
            "login"


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (toString route)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        ]
