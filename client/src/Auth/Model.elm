module Auth.Model exposing (Model)

import Auth.Api exposing (User)
import RemoteData exposing (WebData)


type alias Model =
    { jwt : Maybe String
    , user : Maybe User
    , fetchUserResponse : WebData User
    }
