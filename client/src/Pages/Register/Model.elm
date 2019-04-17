module Pages.Register.Model exposing (Model)

import Pages.Register.Api exposing (RegisterResponseError)
import Utils.ErrorResponse exposing (ResponseData)


type alias Model =
    { username : String
    , password : String
    , showErrors : Bool
    , registerResponse : ResponseData RegisterResponseError ()
    }
