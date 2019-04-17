module Pages.CreatePlaylist.Selectors exposing
    ( getPlaylistName
    , getPlaylistStyle
    , getPlaylistTagInputValue
    , getPlaylistTags
    , getPrivacyOption
    )

import Pages.CreatePlaylist.Model as CreatePlaylist exposing (PlaylistPrivacy, PlaylistStyle)


getPlaylistName : CreatePlaylist.Model -> String
getPlaylistName model =
    model.playlistName


getPlaylistTagInputValue : CreatePlaylist.Model -> String
getPlaylistTagInputValue model =
    model.playlistTagInput


getPlaylistTags : CreatePlaylist.Model-> List String
getPlaylistTags model =
    model.playlistTags


getPrivacyOption : CreatePlaylist.Model -> PlaylistPrivacy
getPrivacyOption model =
    model.playlistPrivacy


getPlaylistStyle : CreatePlaylist.Model -> PlaylistStyle
getPlaylistStyle model =
    model.playlistStyle
