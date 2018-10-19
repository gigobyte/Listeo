module Pages.Login.Api exposing (LoginRequestData, getLoginRequestModel)

import Pages.Login as Login


type alias LoginRequestData =
    { username : String
    , password : String
    }
