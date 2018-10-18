module Pages.Login.Api exposing (LoginRequestData)


type alias LoginRequestData =
    { username : String
    , password : String
    }
