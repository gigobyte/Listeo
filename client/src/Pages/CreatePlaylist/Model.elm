module Pages.CreatePlaylist.Model exposing (Model, PlaylistStyle(..), PrivacyOption(..))

import UI.TagInput exposing (Tag)


type PrivacyOption
    = Public
    | Private


type PlaylistStyle
    = Unordered
    | Ranked


type alias Model =
    { playlistName : String
    , playlistTagInput : String
    , playlistTags : List Tag
    , privacyOption : PrivacyOption
    , playlistStyle : PlaylistStyle
    }
