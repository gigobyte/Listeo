module Pages.CreatePlaylist.Selectors exposing
    ( getPlaylistName
    , getPlaylistStyle
    , getPlaylistTagInputValue
    , getPlaylistTags
    , getPrivacyOption
    )

import Model exposing (AppModel)
import Pages.CreatePlaylist.Model exposing (PlaylistStyle, PlaylistPrivacy)
import UI.TagInput exposing (Tag)


getPlaylistName : AppModel -> String
getPlaylistName model =
    model.createPlaylist.playlistName


getPlaylistTagInputValue : AppModel -> String
getPlaylistTagInputValue model =
    model.createPlaylist.playlistTagInput


getPlaylistTags : AppModel -> List Tag
getPlaylistTags model =
    model.createPlaylist.playlistTags


getPrivacyOption : AppModel -> PlaylistPrivacy
getPrivacyOption model =
    model.createPlaylist.playlistPrivacy


getPlaylistStyle : AppModel -> PlaylistStyle
getPlaylistStyle model =
    model.createPlaylist.playlistStyle
