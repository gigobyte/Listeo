module Pages.CreatePlaylist exposing (init, update, view)

import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.CreatePlaylist.Model exposing (Model)
import Pages.CreatePlaylist.Selectors as Selectors
import Pages.CreatePlaylist.Update as Update
import Pages.CreatePlaylist.View as View
import Session exposing (Session)
import Utils.Styles exposing (StyledDocument)


view : AppModel -> StyledDocument Msg
view model =
    { title = "Create playlist - Listeo"
    , body =
        [ View.view
            { playlistNameValue = Selectors.getPlaylistName model
            , playlistNameError = Nothing
            , playlistTagInput = Selectors.getPlaylistTagInputValue model
            , playlistTags = Selectors.getPlaylistTags model
            , playlistPrivacy = Selectors.getPrivacyOption model
            , playlistStyle = Selectors.getPlaylistStyle model
            }
        ]
    }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Update.update msg model.createPlaylist session


init : Model
init =
    Update.init
