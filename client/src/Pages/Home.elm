module Pages.Home exposing (Model, init, title, update, view)

import Pages.Home.Model as Model
import Pages.Home.Update as Update
import Pages.Home.View as View


type alias Model =
    Model.Model


view =
    View.view


update =
    Update.update


init =
    Update.init


title : Model -> String
title _ =
    "Home - Listeo"
