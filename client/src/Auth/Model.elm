module Auth.Model exposing (Model)

import Auth.Api exposing (User)
import Utils.ErrorResponse exposing (ResponseData)
import Utils.Fetch exposing (Token)


type alias Model =
    { jwt : Token
    , user : ResponseData () User
    }
