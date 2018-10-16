module Routes exposing (Route(..), parser)

import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | Login


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        ]
