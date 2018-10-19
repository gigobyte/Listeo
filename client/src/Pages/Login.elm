module Pages.Login exposing (Model, init, update, view)

import Pages.Login.Model as Model
import Pages.Login.Update as Update
import Pages.Login.View as View


type alias Model =
    Model.Model


view =
    View.view


update =
    Update.update


init =
    Update.init
