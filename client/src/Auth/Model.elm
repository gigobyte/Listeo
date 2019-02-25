module Auth.Model exposing (Model)

import Auth.Api exposing (User)
import RemoteData exposing (WebData)
import Utils.Fetch exposing (Token)


type alias Model =
    { jwt : Token
    , user : WebData User
    }
