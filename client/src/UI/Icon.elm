module UI.Icon exposing
    ( cloudDownload
    , cloudUpload
    , folderPlus
    , plusCircle
    )

import Css exposing (..)
import Html.Styled exposing (Html, i, styled)
import Html.Styled.Attributes exposing (class)
import Utils.Styles exposing (StyledElement)


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


cloudUpload : StyledElement msg
cloudUpload =
    icon "fas fa-cloud-upload-alt"


cloudDownload : StyledElement msg
cloudDownload =
    icon "fas fa-cloud-download-alt"
