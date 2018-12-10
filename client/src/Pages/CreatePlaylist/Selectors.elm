module Pages.CreatePlaylist.Selectors exposing
    ( getPlaylistNameValue
    , getPlaylistTagInputValue
    , getPlaylistTags
    )

import Model exposing (AppModel)
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
