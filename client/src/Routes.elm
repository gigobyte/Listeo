module Routes exposing (Route(..), href, parser, pushUrl)

import Browser.Navigation as Nav
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | Login
    | Register


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Login ->
            "login"

        Register ->
            "register"


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (toString route)


href : Route -> Attribute msg
href =
    toString >> Attributes.href


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        ]
