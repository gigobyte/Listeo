module Pages.Playlist.PlaylistPrivacy exposing (PlaylistPrivacy(..), decoder, encode)

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


encode =
    playlistPrivacyEnum.encode


decoder =
    playlistPrivacyEnum.decoder
