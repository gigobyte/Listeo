module Auth.Selectors exposing (getUser)

import Auth.Api exposing (User)
import Model exposing (AppModel)


getUser : AppModel -> Maybe User
getUser model =
    model.auth.user
