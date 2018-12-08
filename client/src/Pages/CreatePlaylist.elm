module Pages.CreatePlaylist exposing (init, title, update, view)

import Html.Styled exposing (Html)
import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.CreatePlaylist.Model exposing (Model)
import Pages.CreatePlaylist.Update as Update
import Pages.CreatePlaylist.View as View


view : AppModel -> Html Msg
view model =
    View.view {}


update : Msg -> AppModel -> ( Model, Cmd Msg )
update msg model =
    Update.update msg model.createPlaylist


init =
    Update.init


title : AppModel -> String
title _ =
    "Create playlist - Listeo"
