module Pages.Home exposing (init, update, view)

import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Home.Model exposing (Model)
import Pages.Home.Update as Home
import Pages.Home.View as Home
import Session exposing (Session)
import Utils.Styles exposing (StyledDocument)


view : AppModel -> StyledDocument Msg
view _ =
    { title = "Home - Listeo"
    , body = [ Home.view {} ]
    }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Home.update msg model.home session


init : Model
init =
    Home.init
