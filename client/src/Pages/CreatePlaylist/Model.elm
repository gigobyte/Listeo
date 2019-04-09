module Pages.CreatePlaylist.Model exposing
    ( Model
    , PlaylistPrivacy(..)
    , PlaylistStyle(..)
    , playlistPrivacyToString
    , playlistStyleToString
    )


type PlaylistPrivacy
    = Public
    | Private


playlistPrivacyToString : PlaylistPrivacy -> String
playlistPrivacyToString privacy =
    case privacy of
        Public ->
            "Public"

        Private ->
            "Private"


type PlaylistStyle
    = Unordered
    | Ranked


playlistStyleToString : PlaylistStyle -> String
playlistStyleToString style =
    case style of
        Unordered ->
            "Unordered"

        Ranked ->
            "Ranked"


type alias Model =
    { playlistName : String
    , playlistDescription : String
    , playlistTagInput : String
    , playlistTags : List String
    , playlistPrivacy : PlaylistPrivacy
    , playlistStyle : PlaylistStyle
    , showErrors : Bool
    }
