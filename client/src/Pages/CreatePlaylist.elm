module Pages.CreatePlaylist exposing (Model, init, title, update, view)

import Pages.CreatePlaylist.Model as Model
import Pages.CreatePlaylist.Update as Update
import Pages.CreatePlaylist.View as View


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
    "Create playlist - Listeo"
