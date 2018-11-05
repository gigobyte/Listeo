module Pages.Login.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Maybe.Extra as Maybe
import Msg exposing (Msg(..))
import Pages.Header as Header
import Pages.Login.Api as Api exposing (LoginResponse(..))
import Pages.Login.Model exposing (Model)
import Pages.Login.Selectors exposing (getValidationErrors)
import Pages.Login.Validation as Validation exposing (LoginField(..), LoginValidationError(..))
import RemoteData exposing (RemoteData(..))
import Routes
import UI.Button as Button
import UI.Container as Container
import UI.Input as Input
import UI.Link as Link
import Utils.StyleTypes exposing (StyledElement)
import Utils.Validation exposing (getErrorForField)


container : StyledElement msg
container =
    styled div
        [ height <| pct 100
        ]


loginForm : StyledElement msg
loginForm =
    styled (Container.centered form)
        [ height <| pct 66
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
        , marginBottom <| px 15
        , fontSize <| rem 1
        ]


viewLoginResponse : LoginResponse -> Html msg
viewLoginResponse res =
    case res of
        ErrorResponse { errorDescription } ->
            text <| Api.loginErrorToString errorDescription

        _ ->
            text ""


view : Model -> Html Msg
view model =
    let
        validationErrors =
            getValidationErrors model

        usernameError =
            getErrorForField Username validationErrors
                |> Maybe.map Validation.errToString

        passwordError =
            getErrorForField Password validationErrors
                |> Maybe.map Validation.errToString
    in
    Container.fullHeight div
        []
        [ Header.view
        , loginForm [ onSubmit LoginAttempted ]
            [ title [] [ text "Sign In" ]
            , Input.view
                { validationError = usernameError
                , inputAttributes =
                    [ placeholder "Username"
                    , value model.username
                    , onInput LoginUsernameUpdated
                    ]
                }
                []
                []
            , Input.view
                { validationError = passwordError
                , inputAttributes =
                    [ placeholder "Password"
                    , type_ "password"
                    , value model.password
                    , onInput LoginPasswordUpdated
                    ]
                }
                []
                []
            , submitButton
                [ type_ "submit"
                ]
                [ text "Let's go!" ]
            , case model.loginResponse of
                Success res ->
                    viewLoginResponse res

                _ ->
                    text ""
            , Link.view { to = Routes.Register } [] [ text "Don't have an account?" ]
            ]
        ]
