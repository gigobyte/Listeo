module Pages.CreatePlaylist.Selectors exposing
    ( getPlaylistName
    , getPlaylistStyle
    , getPlaylistTagInputValue
    , getPlaylistTags
    , getPrivacyOption
    )

import Model exposing (AppModel)
import Pages.CreatePlaylist.Model exposing (PlaylistPrivacy, PlaylistStyle)


getPlaylistName : AppModel -> String
getPlaylistName model =
    model.createPlaylist.playlistName


getPlaylistTagInputValue : AppModel -> String
getPlaylistTagInputValue model =
    model.createPlaylist.playlistTagInput


getPlaylistTags : AppModel -> List String
getPlaylistTags model =
    model.createPlaylist.playlistTags


getPrivacyOption : AppModel -> PlaylistPrivacy
getPrivacyOption model =
    model.createPlaylist.playlistPrivacy


getPlaylistStyle : AppModel -> PlaylistStyle
getPlaylistStyle model =
    model.createPlaylist.playlistStyle
