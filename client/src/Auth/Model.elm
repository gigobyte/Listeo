module Auth.Model exposing (Model)

import Auth.Api exposing (User)
import Auth.Token exposing (Token)
import RemoteData exposing (WebData)


type alias Model =
    { jwt : Token
    , user : WebData User
    }
