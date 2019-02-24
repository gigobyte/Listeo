module Pages.Header exposing (init, update, view)

import Auth.Selectors as Selectors
import Session exposing (Session)
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


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Update.update msg model.header session


init : Model
init =
    Update.init
