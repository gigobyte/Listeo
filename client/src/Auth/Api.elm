module Auth.Api exposing (User, fetchUser)

import Http exposing (expectJson)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.Fetch as Fetch


type alias User =
    { username : String
    , createdOn : String
    }


fetchUser : String -> String -> Cmd (WebData User)
fetchUser apiRoot token =
    Fetch.getWithAuth
        { url = apiRoot ++ "/me"
        , token = token
        , expect = expectJson RemoteData.fromResult userDecoder
        }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "username" string
        |> required "createdOn" string
