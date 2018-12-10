module Pages.CreatePlaylist.Model exposing (Model)

import UI.TagInput exposing (Tag)


type alias Model =
    { playlistName : String
    , playlistTagInput : String
    , playlistTags : List Tag
    }
