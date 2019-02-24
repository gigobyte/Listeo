module Pages.Header.Selectors exposing (shouldShowAddPlaylistButton)

import Model exposing (AppModel)
import Route


shouldShowAddPlaylistButton : AppModel -> Bool
shouldShowAddPlaylistButton model =
    model.route /= Route.CreatePlaylist
