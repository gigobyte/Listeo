module Auth.Token exposing (Token(..), empty, from, toString)


type Token
    = Token (Maybe String)


toString : Token -> String
toString (Token maybeStr) =
    case maybeStr of
        Just str ->
            str

        Nothing ->
            ""


from : String -> Token
from str =
    Token (Just str)


empty : Token
empty =
    Token Nothing
