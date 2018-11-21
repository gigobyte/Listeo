module Model exposing (AppModel)

import Auth.Model as Auth
import Browser.Navigation as Nav
import Pages.Home as Home
import Pages.Login as Login
import Pages.Register as Register
import Route exposing (Route)


type alias AppModel =
    { key : Nav.Key
    , url : Route
    , login : Login.Model
    , register : Register.Model
    , auth : Auth.Model
    , home : Home.Model
    }
