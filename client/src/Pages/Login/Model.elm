module Pages.Login.Model exposing (Model)

import Pages.Login.Api exposing (LoginResponse, LoginResponseError)
import Utils.ErrorResponse exposing (ResponseData)


type alias Model =
    { username : String
    , password : String
    , showErrors : Bool
    , loginResponse : ResponseData LoginResponseError LoginResponse
    }
