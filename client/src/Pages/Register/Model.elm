module Pages.Register.Model exposing (Model)

import Pages.Register.Api exposing (RegisterResponse)
import RemoteData exposing (WebData)


type alias Model =
    { username : String
    , password : String
    , showErrors : Bool
    , registerResponse : WebData RegisterResponse
    }
