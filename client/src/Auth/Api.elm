module Auth.Api exposing (User, fetchUser)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Utils.ErrorResponse exposing (ResponseData, expectJsonWithError)
import Utils.Fetch as Fetch exposing (ApiRoot, Token)


type alias User =
    { username : String
    , createdOn : String
    }


fetchUser : ApiRoot -> Token -> Cmd (ResponseData () User)
fetchUser apiRoot token =
    Fetch.getWithAuth
        { url = Fetch.currentUser apiRoot
        , token = token
        , expect = expectJsonWithError (Decode.succeed ()) userDecoder
        }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "username" string
        |> required "createdOn" string
