module Model exposing (AppModel)

import Pages.Login as Login
import Pages.Register as Register
import Auth as Auth
import Browser.Navigation as Nav
import Routes exposing (Route)

type alias AppModel =
    { key : Nav.Key
    , url : Maybe Route
    , login : Login.Model
    , register : Register.Model
    , auth : Auth.Model
    }