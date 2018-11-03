module Pages.Login.Model exposing (Model)

import Pages.Login.Api exposing (LoginResponse)
import RemoteData exposing (WebData)


type alias Model =
    { username : String
    , password : String
    , showErrors : Bool
    , loginResponse : WebData LoginResponse
    }
