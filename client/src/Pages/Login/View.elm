module Pages.Login.View exposing (view)

import Css exposing (..)
import Css.Transitions as Transitions exposing (easeIn, transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Maybe.Extra as Maybe
import Msg exposing (Msg(..))
import Pages.Login.Model exposing (Model)
import Pages.Login.Selectors exposing (getValidationErrors)
import Pages.Login.Validation exposing (LoginField(..), LoginValidationError(..), errToString)
import UI.Button as Button
import UI.Colors exposing (gray100, red100)
import UI.Input as Input
import Utils.StyleTypes exposing (StyledElement)
import Utils.Validation exposing (getErrorForField)


container : StyledElement msg
container =
    styled div
        [ displayFlex
        , justifyContent center
        , alignItems center
        , flexDirection column
        , height <| pct 70
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
    let
        validationErrors =
            getValidationErrors model

        usernameError =
            getErrorForField Username validationErrors
                |> Maybe.map errToString

        passwordError =
            getErrorForField Password validationErrors
                |> Maybe.map errToString
    in
    container []
        [ title [] [ text "Login" ]
        , Input.view
            { validationError = usernameError
            , inputAttributes =
                [ placeholder "Username"
                , value model.username
                , onInput LoginUsernameUpdated
                ]
            }
        , Input.view
            { validationError = passwordError
            , inputAttributes =
                [ placeholder "Password"
                , value model.password
                , onInput LoginPasswordUpdated
                ]
            }
        , submitButton
            [ type_ "submit"
            , onClick LoginAttempted
            ]
            [ text "Let's go!" ]
        ]
