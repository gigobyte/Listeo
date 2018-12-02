module Pages.AddPlaylist exposing (Model, init, title, update, view)

import Pages.AddPlaylist.Model as Model
import Pages.AddPlaylist.Update as Update
import Pages.AddPlaylist.View as View


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
    "Add playlist - Listeo"
