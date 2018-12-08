module Model exposing (AppModel)

import Auth.Model as Auth
import Browser.Navigation as Nav
import Pages.CreatePlaylist as CreatePlaylist
import Pages.Header as Header
import Pages.Home as Home
import Pages.Login.Model as Login
import Pages.Register.Model as Register
import Route exposing (Route)


type alias AppModel =
    { key : Nav.Key
    , url : Route
    , login : Login.Model
    , register : Register.Model
    , auth : Auth.Model
    , home : Home.Model
    , createPlaylist : CreatePlaylist.Model
    , header : Header.Model
    }
