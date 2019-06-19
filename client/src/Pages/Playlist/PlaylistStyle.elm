module Pages.Playlist.PlaylistStyle exposing (PlaylistStyle(..), decoder, encode)

import Enum exposing (Enum)


type PlaylistStyle
    = Unordered
    | Ranked


playlistStyleEnum : Enum PlaylistStyle
playlistStyleEnum =
    Enum.create
        [ ( "Unordered", Unordered )
        , ( "Ranked", Ranked )
        ]


encode =
    playlistStyleEnum.encode


decoder =
    playlistStyleEnum.decoder
