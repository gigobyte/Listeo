module Pages.CreatePlaylist.Model exposing (Model, PrivacyOption(..))

import UI.TagInput exposing (Tag)


type PrivacyOption
    = Public
    | Private


type alias Model =
    { playlistName : String
    , playlistTagInput : String
    , playlistTags : List Tag
    , privacyOption : PrivacyOption
    }
