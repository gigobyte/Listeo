module Pages.Register.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Maybe.Extra as Maybe
import Msg exposing (Msg(..))
import Pages.Register.Model exposing (Model)
import Pages.Register.Selectors as Selectors
import Pages.Register.Validation as Validation exposing (RegisterField(..), RegisterValidationError(..))
import Route
import UI.Button as Button
import UI.Container as Container
import UI.Error as Error
import UI.Input as Input
import UI.Link as Link
import Utils.Styles exposing (StyledElement)
import Utils.Validation exposing (getErrorForField)


registerForm : StyledElement msg
registerForm =
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


view : Model -> Html Msg
view model =
    let
        validationErrors =
            Selectors.getValidationErrors model

        usernameError =
            getErrorForField Username validationErrors
                |> Maybe.map Validation.errToString

        passwordError =
            getErrorForField Password validationErrors
                |> Maybe.map Validation.errToString
    in
    Container.fullHeight div
        []
        [ registerForm [ onSubmit RegisterAttempted ]
            [ title [] [ text "Register" ]
            , Input.view
                { validationError = usernameError
                , inputAttributes =
                    [ placeholder "Username"
                    , value model.username
                    , onInput RegisterUsernameUpdated
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
                    , onInput RegisterPasswordUpdated
                    ]
                }
                []
                []
            , submitButton
                [ type_ "submit"
                , disabled <| Selectors.isSubmitButtonDisabled model
                ]
                [ text "Beam me up!" ]
            , Error.text { error = Selectors.getRegisterRequestErrorText model } [] []
            , Link.view { to = Route.Login } [] [ text "Already have an account?" ]
            ]
        ]
