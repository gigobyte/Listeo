module Pages.AddPlaylist.AddPlaylistOverlay exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Msg exposing (Msg(..))
import Pages.AddPlaylist.Model exposing (Model)
import UI.Button as Button
import UI.Colors exposing (blue150)
import UI.Container as Container
import UI.Icon as Icon
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled div
        [ displayFlex
        , paddingTop <| pct 5
        ]


header : StyledElement msg
header =
    styled div
        [ textAlign center
        , paddingTop <| pct 5
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
    styled (Container.centered div)
        [ flex <| int 1
        , padding2 zero (px 10)
        ]


optionIcon : StyledElement msg -> StyledElement msg
optionIcon icon =
    styled icon
        [ textAlign center
        , fontSize <| rem 5
        , color blue150
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
    Container.fullHeight div
        []
        [ header []
            [ div [] [ title [] [ text "New playlist" ] ]
            , div [] [ subtitle [] [ text "Choose a starting point" ] ]
            ]
        , container []
            [ optionCard [] []
            , optionCard []
                [ optionIcon Icon.folderPlus [] []
                , optionDescription []
                    [ text "Start with an empty playlist. "
                    ]
                , optionButton [] [ text "Create new" ]
                ]
            , optionCard []
                [ optionIcon Icon.cloudDownload [] []
                , optionDescription []
                    [ text "Import your existing playlist."
                    ]
                , optionButton [] [ text "Import" ]
                ]
            , optionCard [] []
            ]
        ]
