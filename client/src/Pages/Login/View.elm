module Pages.Login.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Msg exposing (Msg(..))
import Pages.Login.Model exposing (Model)
import UI.Button as Button
import UI.Input as Input
import Utils.StyleTypes exposing (StyledElement)


container : StyledElement msg
container =
    styled div
        [ displayFlex
        , justifyContent center
        , alignItems center
        , flexDirection column
        , height <| pct 70
        ]


loginInput : StyledElement msg
loginInput =
    styled Input.view
        [ marginBottom <| px 10
        , fontSize <| px 16
        ]


title : StyledElement msg
title =
    styled h1
        [ fontSize <| rem 3
        ]


submitButton : StyledElement msg
submitButton =
    styled Button.view
        [ marginTop <| px 10
        , fontSize <| rem 1
        ]


view : Model -> Html Msg
view model =
    container []
        [ title [] [ text "Login" ]
        , loginInput [ type_ "text", placeholder "Username", value model.username, onInput LoginUsernameUpdated ] []
        , loginInput [ type_ "text", placeholder "Password", value model.password, onInput LoginPasswordUpdated ] []
        , submitButton [ type_ "submit", onClick LoginAttempted ] [ text "Let's go!" ]
        ]
