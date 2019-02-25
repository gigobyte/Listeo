module Model exposing (AppModel)

import Auth.Model as Auth
import Browser.Navigation as Nav
import Pages.CreatePlaylist.Model as CreatePlaylist
import Pages.Header.Model as Header
import Pages.Home.Model as Home
import Pages.Login.Model as Login
import Pages.Register.Model as Register
import Route exposing (Route)
import Utils.Fetch exposing (ApiRoot)


type alias AppModel =
    { key : Nav.Key
    , route : Route
    , login : Login.Model
    , register : Register.Model
    , auth : Auth.Model
    , home : Home.Model
    , createPlaylist : CreatePlaylist.Model
    , header : Header.Model
    , apiRoot : ApiRoot
    }
