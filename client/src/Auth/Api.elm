module Auth.Api exposing (User, fetchUser)

import Http exposing (expectJson)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.Fetch as Fetch exposing (ApiRoot, Token)


type alias User =
    { username : String
    , createdOn : String
    }


fetchUser : ApiRoot -> Token -> Cmd (WebData User)
fetchUser apiRoot token =
    Fetch.getWithAuth
        { url = Fetch.currentUser apiRoot
        , token = token
        , expect = expectJson RemoteData.fromResult userDecoder
        }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "username" string
        |> required "createdOn" string
