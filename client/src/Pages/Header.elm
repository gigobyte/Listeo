module Pages.Header exposing (Model, init, update, view)

import Pages.Header.Model as Model
import Pages.Header.Update as Update
import Pages.Header.View as View


type alias Model =
    Model.Model


view =
    View.view


update =
    Update.update


init =
    Update.init
