module Pages.Header.AddPlaylistModal exposing (view)

import Css exposing (..)
import Css.Global exposing (children, typeSelector)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
import Session exposing (Msg(..))
import UI.Button as Button
import UI.Colors exposing (blue200)
import UI.Container as Container
import UI.Icon as Icon
import UI.Modal as Modal
import Utils.Styles exposing (StyledElement)


viewContainer : StyledElement msg
viewContainer =
    styled (Container.viewFullHeight div)
        [ displayFlex
        , flexDirection column
        ]


viewOptionsContainer : StyledElement msg
viewOptionsContainer =
    styled div
        [ displayFlex
        , paddingTop <| pct 5
        , height <| pct 100
        , justifyContent center
        ]


viewHeader : StyledElement msg
viewHeader =
    styled div
        [ textAlign center
        ]


viewTitle : StyledElement msg
viewTitle =
    styled span
        [ textTransform uppercase
        , fontSize <| rem 0.8
        ]


viewSubtitle : StyledElement msg
viewSubtitle =
    styled span
        [ fontSize <| rem 1.4
        ]


viewOptionCard : StyledElement msg
viewOptionCard =
    styled div
        [ flexBasis <| pct 25
        , displayFlex
        , flexDirection column
        , alignItems center
        , padding2 zero (px 10)
        , hover
            [ children
                [ typeSelector "i"
                    [ transform <| scale 1.1
                    ]
                ]
            ]
        ]


optionIcon : StyledElement msg -> StyledElement msg
optionIcon icon =
    styled icon
        [ textAlign center
        , fontSize <| rem 5
        , color blue200
        , transition [ Transitions.transform 300 ]
        ]


viewOptionDescription : StyledElement msg
viewOptionDescription =
    styled div
        [ marginTop <| px 20
        , marginBottom <| px 25
        , textAlign center
        ]


view : Html Session.Msg
view =
    Modal.view { onClose = AddPlaylistModalClosed }
        []
        [ viewContainer
            []
            [ viewHeader []
                [ div [] [ viewTitle [] [ text "New playlist" ] ]
                , div [] [ viewSubtitle [] [ text "Choose a starting point" ] ]
                ]
            , viewOptionsContainer []
                [ viewOptionCard []
                    [ optionIcon Icon.folderPlus [] []
                    , viewOptionDescription []
                        [ text "Start with an empty playlist. "
                        ]
                    , Button.view [ onClick CreateNewPlaylistSelected ] [ text "Create new" ]
                    ]
                , viewOptionCard []
                    [ optionIcon Icon.cloudDownload [] []
                    , viewOptionDescription []
                        [ text "Import your existing playlist."
                        ]
                    , Button.view [ Attributes.disabled True ] [ text "Import" ]
                    ]
                ]
            ]
        ]
