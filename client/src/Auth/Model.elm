module Auth.Model exposing (Model)

import Auth.Api exposing (User)

type alias Model =
    { jwt : Maybe String
    , user : Maybe User
    }
