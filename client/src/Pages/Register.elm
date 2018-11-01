module Pages.Register exposing (Model, init, update, view)

import Pages.Register.Model as Model
import Pages.Register.Update as Update
import Pages.Register.View as View


type alias Model =
    Model.Model


view =
    View.view


update =
    Update.update


init =
    Update.init
