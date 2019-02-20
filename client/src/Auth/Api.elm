module Auth.Api exposing (User, fetchUser)

import Http exposing (expectJson)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.Api exposing (endpoint)
import Utils.Fetch as Fetch


type alias User =
    { username : String
    , createdOn : String
    }


fetchUser : String -> Cmd (WebData User)
fetchUser token =
    Fetch.getWithAuth
        { url = endpoint "/me"
        , token = token
        , expect = expectJson RemoteData.fromResult userDecoder
        }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "username" string
        |> required "createdOn" string
