module Pages.ViewPlaylist exposing (Model)

-- MODEL

import ErrorResponse exposing (ResponseData)
import Time


type alias Model =
    { playlist : ResponseData () Playlist
    }


type alias Playlist =
    { id : String
    , style : PlaylistStyle
    , name : String
    , privacy : PlaylistPrivacy
    , createdOn : Time.Posix
    , tags : List PlaylistTag
    }


type alias PlaylistTag =
    { id : String
    , name : String
    , createdOn : Time.Posix
    }



-- VIEW
-- UPDATE
-- EXPORTS
