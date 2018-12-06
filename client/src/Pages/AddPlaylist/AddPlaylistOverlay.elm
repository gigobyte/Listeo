module Pages.AddPlaylist.AddPlaylistOverlay exposing (view)

import Css exposing (..)
import Css.Global exposing (children, typeSelector)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
import Msg exposing (Msg(..))
import Pages.AddPlaylist.Model exposing (Model)
import Route exposing (pushUrl)
import UI.Button as Button
import UI.Colors exposing (blue150)
import UI.Container as Container
import UI.Icon as Icon
import UI.Link as Link
import UI.Modal as Modal
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled (Container.fullHeight div)
        [ displayFlex
        , flexDirection column
        ]


optionsContainer : StyledElement msg
optionsContainer =
    styled div
        [ displayFlex
        , paddingTop <| pct 5
        , height <| pct 100
        , justifyContent center
        ]


header : StyledElement msg
header =
    styled div
        [ textAlign center
        ]


title : StyledElement msg
title =
    styled span
        [ textTransform uppercase
        , fontSize <| rem 0.8
        ]


subtitle : StyledElement msg
subtitle =
    styled span
        [ fontSize <| rem 1.4
        ]


optionCard : StyledElement msg
optionCard =
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
        , color blue150
        , transition [ Transitions.transform 300 ]
        ]


optionDescription : StyledElement msg
optionDescription =
    styled div
        [ marginTop <| px 20
        , marginBottom <| px 25
        , textAlign center
        ]


optionButton : StyledElement msg
optionButton =
    styled Button.view
        [ fontSize <| rem 1
        ]


view : Model -> Html Msg
view model =
    Modal.view { onClose = AddPlaylistModalClosed }
        []
        [ container
            []
            [ header []
                [ div [] [ title [] [ text "New playlist" ] ]
                , div [] [ subtitle [] [ text "Choose a starting point" ] ]
                ]
            , optionsContainer []
                [ optionCard []
                    [ optionIcon Icon.folderPlus [] []
                    , optionDescription []
                        [ text "Start with an empty playlist. "
                        ]
                    , optionButton [ onClick CreateNewPlaylistSelected ] [ text "Create new" ]
                    ]
                , optionCard []
                    [ optionIcon Icon.cloudDownload [] []
                    , optionDescription []
                        [ text "Import your existing playlist."
                        ]
                    , optionButton [ Attributes.disabled True ] [ text "Import" ]
                    ]
                ]
            ]
        ]
