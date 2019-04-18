module Pages.CreatePlaylist exposing (init, update, view)

import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.CreatePlaylist.Model exposing (Model)
import Pages.CreatePlaylist.Selectors as Selector
import Pages.CreatePlaylist.Update as CreatePlaylist
import Pages.CreatePlaylist.View as CreatePlaylist
import Session exposing (Session)
import Utils.Styles exposing (StyledDocument)


view : AppModel -> StyledDocument Msg
view model =
    { title = "Create playlist - Listeo"
    , body =
        [ CreatePlaylist.view
            { playlistNameValue = Selector.getPlaylistName model.createPlaylist
            , playlistNameError = Selector.getPlaylistNameError model.createPlaylist
            , playlistTagInput = Selector.getPlaylistTagInputValue model.createPlaylist
            , playlistTags = Selector.getPlaylistTags model.createPlaylist
            , playlistPrivacy = Selector.getPrivacyOption model.createPlaylist
            , playlistStyle = Selector.getPlaylistStyle model.createPlaylist
            }
        ]
    }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    CreatePlaylist.update msg model.createPlaylist session


init : Model
init =
    CreatePlaylist.init
