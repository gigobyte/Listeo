module Pages.CreatePlaylist.Model exposing
    ( Model
    , PlaylistPrivacy(..)
    , PlaylistStyle(..)
    , playlistPrivacyEnum
    , playlistStyleEnum
    )

import Enum exposing (Enum)


type PlaylistPrivacy
    = Public
    | Private


playlistPrivacyEnum : Enum PlaylistPrivacy
playlistPrivacyEnum =
    Enum.create
        [ ( "Public", Public )
        , ( "Private", Private )
        ]


type PlaylistStyle
    = Unordered
    | Ranked


playlistStyleEnum : Enum PlaylistStyle
playlistStyleEnum =
    Enum.create
        [ ( "Unordered", Unordered )
        , ( "Ranked", Ranked )
        ]


type alias Model =
    { playlistName : String
    , playlistDescription : String
    , playlistTagInput : String
    , playlistTags : List String
    , playlistPrivacy : PlaylistPrivacy
    , playlistStyle : PlaylistStyle
    , showErrors : Bool
    }
