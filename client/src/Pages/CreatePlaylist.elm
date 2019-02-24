module Pages.CreatePlaylist exposing (init, title, update, view)

import Session exposing (Session)
import Html.Styled exposing (Html)
import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.CreatePlaylist.Model exposing (Model)
import Pages.CreatePlaylist.Selectors as Selectors
import Pages.CreatePlaylist.Update as Update
import Pages.CreatePlaylist.View as View


view : AppModel -> Html Msg
view model =
    View.view
        { playlistNameValue = Selectors.getPlaylistName model
        , playlistNameError = Nothing
        , playlistTagInput = Selectors.getPlaylistTagInputValue model
        , playlistTags = Selectors.getPlaylistTags model
        , playlistPrivacy = Selectors.getPrivacyOption model
        , playlistStyle = Selectors.getPlaylistStyle model
        }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Update.update msg model.createPlaylist session


init : Model
init =
    Update.init


title : AppModel -> String
title _ =
    "Create playlist - Listeo"
