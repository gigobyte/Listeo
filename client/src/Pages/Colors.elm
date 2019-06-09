module Pages.Colors exposing (view)

-- This is a debug page

import Css exposing (..)
import Html.Styled exposing (..)
import Styles exposing (StyledDocument, StyledElement)
import UI.Colors exposing (..)
import UI.Container as Container


viewContainer : StyledElement msg
viewContainer =
    styled div
        [ displayFlex
        , justifyContent spaceAround
        ]


viewColorContainer : StyledElement msg
viewColorContainer =
    styled div
        [ displayFlex
        , flexDirection column
        , alignItems center
        , paddingBottom <| px 25
        , paddingRight <| px 25
        , minWidth <| px 110
        ]


viewColor : Color -> Html msg
viewColor color =
    styled div
        [ height <| px 25
        , width <| px 25
        , backgroundColor color
        ]
        []
        []


view : StyledDocument msg
view =
    { title = "Colors page - DEBUG ONLY"
    , body =
        [ Container.viewCentered div
            []
            [ viewContainer []
                [ viewColorContainer []
                    [ text "white", viewColor white ]
                , viewColorContainer []
                    [ text "black", viewColor black ]
                ]
            , viewContainer []
                [ viewColorContainer []
                    [ text "gray100", viewColor gray100 ]
                , viewColorContainer []
                    [ text "gray200", viewColor gray200 ]
                , viewColorContainer []
                    [ text "gray300", viewColor gray300 ]
                , viewColorContainer []
                    [ text "gray400", viewColor gray400 ]
                ]
            , viewContainer []
                [ viewColorContainer []
                    [ text "blue100", viewColor blue100 ]
                , viewColorContainer []
                    [ text "blue200", viewColor blue200 ]
                , viewColorContainer []
                    [ text "blue300", viewColor blue300 ]
                , viewColorContainer []
                    [ text "blue400", viewColor blue400 ]
                ]
            , viewContainer []
                [ viewColorContainer []
                    [ text "crimson100", viewColor crimson100 ]
                , viewColorContainer []
                    [ text "crimson200", viewColor crimson200 ]
                , viewColorContainer []
                    [ text "crimson300", viewColor crimson300 ]
                , viewColorContainer []
                    [ text "crimson400", viewColor crimson400 ]
                ]
            ]
        ]
    }
