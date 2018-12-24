module Pages.CreatePlaylist.Selectors exposing
    ( getPlaylistNameValue
    , getPlaylistTagInputValue
    , getPlaylistTags
    , getPrivacyOption
    )

import Model exposing (AppModel)
import Pages.CreatePlaylist.Model exposing (PrivacyOption)
import UI.TagInput exposing (Tag)


getPlaylistNameValue : AppModel -> String
getPlaylistNameValue model =
    model.createPlaylist.playlistName


getPlaylistTagInputValue : AppModel -> String
getPlaylistTagInputValue model =
    model.createPlaylist.playlistTagInput


getPlaylistTags : AppModel -> List Tag
getPlaylistTags model =
    model.createPlaylist.playlistTags


getPrivacyOption : AppModel -> PrivacyOption
getPrivacyOption model =
    model.createPlaylist.privacyOption
