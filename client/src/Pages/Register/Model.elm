module Pages.Register.Model exposing (Model, RegisterError(..), RegisterResponse)

import RemoteData exposing (WebData)


type RegisterError
    = ValidationFailed
    | UserAlreadyExists
    | ServerError


type alias RegisterResponse =
    { errorDescription : Maybe RegisterError
    }


type alias Model =
    { username : String
    , password : String
    , showErrors : Bool
    , registerResponse : WebData RegisterResponse
    }
