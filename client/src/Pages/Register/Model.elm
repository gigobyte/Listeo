module Pages.Register.Model exposing (Model)


type alias Model =
    { username : String
    , password : String
    , showErrors : Bool
    }
