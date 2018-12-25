module Route exposing (Route(..), href, parseUrl, pushUrl)

import Browser.Navigation as Nav
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, parse, s)


type Route
    = Home
    | Login
    | Register
    | About
    | CreatePlaylist
    | NotFound404
    | DebugColors


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Login ->
            "login"

        Register ->
            "register"

        About ->
            "about"

        CreatePlaylist ->
            "create-playlist"

        NotFound404 ->
            "404"

        DebugColors ->
            "colors"


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
        , Parser.map Register (s "register")
        , Parser.map About (s "about")
        , Parser.map CreatePlaylist (s "create-playlist")
        , Parser.map NotFound404 (s "404")
        , Parser.map DebugColors (s "colors")
        ]


parseUrl : Url.Url -> Route
parseUrl =
    parse parser >> Maybe.withDefault NotFound404
