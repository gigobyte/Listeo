module Pages.Home exposing (init, title, update, view)

import Env exposing (Env)
import Html.Styled exposing (Html)
import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Home.Model exposing (Model)
import Pages.Home.Update as Update
import Pages.Home.View as View


view : AppModel -> Html Msg
view _ =
    View.view {}


update : Msg -> AppModel -> Env -> ( Model, Cmd Msg )
update msg model env =
    Update.update msg model.home env


init : Model
init =
    Update.init


title : AppModel -> String
title _ =
    "Home - Listeo"
