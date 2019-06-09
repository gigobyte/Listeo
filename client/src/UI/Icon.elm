module UI.Icon exposing
    ( cloudDownload
    , folderPlus
    , plusCircle
    , times
    )

import Css exposing (..)
import Html.Styled exposing (i, styled)
import Html.Styled.Attributes exposing (class)
import Styles exposing (StyledElement)


icon : String -> StyledElement msg
icon iconClass attrs =
    styled i
        [ width <| pct 100
        ]
        (attrs ++ [ class iconClass ])


plusCircle : StyledElement msg
plusCircle =
    icon "fas fa-plus-circle"


folderPlus : StyledElement msg
folderPlus =
    icon "fas fa-folder-plus"


cloudDownload : StyledElement msg
cloudDownload =
    icon "fas fa-cloud-download-alt"


times : StyledElement msg
times =
    icon "fas fa-times"
