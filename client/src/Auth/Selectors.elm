module Auth.Selectors exposing (getUser)

import Auth.Api exposing (User)
import Auth.Model as Auth
import RemoteData exposing (RemoteData(..))


getUser : Auth.Model -> Maybe User
getUser model =
    case model.user of
        Success user ->
            Just user

        _ ->
            Nothing
