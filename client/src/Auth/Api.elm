module Auth.Api exposing (User, fetchUser)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Jwt
import RemoteData exposing (RemoteData(..), WebData)
import Utils.Api exposing (endpoint)


type alias User =
    { username : String
    , createdOn : String
    }


fetchUser : String -> Cmd (WebData User)
fetchUser token =
    Jwt.get token (endpoint "/me") userDecoder
        |> RemoteData.sendRequest


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "username" string
        |> required "createdOn" string
