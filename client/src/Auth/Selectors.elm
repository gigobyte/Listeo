module Auth.Selectors exposing (getUser)

import Auth.Api exposing (User)
import Model exposing (AppModel)
import RemoteData exposing (RemoteData(..))


getUser : AppModel -> Maybe User
getUser model =
    case model.auth.user of
        Success user ->
            Just user

        _ ->
            Nothing
