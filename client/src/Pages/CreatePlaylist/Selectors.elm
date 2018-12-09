module Pages.CreatePlaylist.Selectors exposing (getPlaylistNameValue)

import Model exposing (AppModel)


getPlaylistNameValue : AppModel -> String
getPlaylistNameValue model =
    model.createPlaylist.playlistName
