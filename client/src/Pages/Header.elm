module Pages.Header exposing (init, update, view)

import Auth.Selectors as Selectors
import Env exposing (Env)
import Html.Styled exposing (Html)
import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Header.Model exposing (Model)
import Pages.Header.Update as Update
import Pages.Header.View as View
import Selectors


view : AppModel -> Html Msg
view model =
    View.view
        { route = Selectors.getRoute model
        , user = Selectors.getUser model
        }


update : Msg -> AppModel -> Env -> ( Model, Cmd Msg )
update msg model env =
    Update.update msg model.header env


init : Model
init =
    Update.init
