module Pages.Header exposing (init, update, view)

import Auth.Selectors as Selector
import Html.Styled exposing (Html)
import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Header.Model exposing (Model)
import Pages.Header.Selectors as Selector
import Pages.Header.Update as Header
import Pages.Header.View as Header
import Session exposing (Session)


view : AppModel -> Html Msg
view model =
    Header.view
        { showAddPlaylistButton = Selector.shouldShowAddPlaylistButton model
        , user = Selector.getUser model.auth
        }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Header.update msg model.header session


init : Model
init =
    Header.init
