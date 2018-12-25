module Pages.Colors exposing (view)

-- This is a debug page

import Css exposing (..)
import Html.Styled exposing (..)
import UI.Colors exposing (..)
import UI.Container as Container
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled div
        [ displayFlex
        , justifyContent spaceAround
        ]


colorContainer : StyledElement msg
colorContainer =
    styled div
        [ displayFlex
        , flexDirection column
        , alignItems center
        , paddingBottom <| px 25
        , paddingRight <| px 25
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


view : Html msg
view =
    Container.centered div
        []
        [ container []
            [ colorContainer []
                [ text "gray300", viewColor gray300 ]
            , colorContainer []
                [ text "gray200", viewColor gray200 ]
            , colorContainer []
                [ text "gray100", viewColor gray100 ]
            ]
        , container []
            [ colorContainer []
                [ text "blue50", viewColor blue50 ]
            , colorContainer []
                [ text "blue100", viewColor blue100 ]
            , colorContainer []
                [ text "blue125", viewColor blue125 ]
            , colorContainer []
                [ text "blue150", viewColor blue150 ]
            , colorContainer []
                [ text "blue200", viewColor blue200 ]
            ]
        , container []
            [ colorContainer []
                [ text "white", viewColor white ]
            , colorContainer []
                [ text "black", viewColor black ]
            ]
        , container []
            [ colorContainer []
                [ text "redOrange100", viewColor redOrange100 ]
            , colorContainer []
                [ text "crimson100", viewColor crimson100 ]
            ]
        ]
