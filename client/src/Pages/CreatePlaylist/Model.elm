module Pages.CreatePlaylist.Model exposing (Model, PlaylistStyle(..), PlaylistPrivacy(..))

import UI.TagInput exposing (Tag)


type PlaylistPrivacy
    = Public
    | Private


type PlaylistStyle
    = Unordered
    | Ranked


type alias Model =
    { playlistName : String
    , playlistTagInput : String
    , playlistTags : List Tag
    , playlistPrivacy : PlaylistPrivacy
    , playlistStyle : PlaylistStyle
    , showErrors : Bool
    }
