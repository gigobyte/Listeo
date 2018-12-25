module Pages.Login.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Maybe.Extra as Maybe
import Msg exposing (Msg(..))
import Pages.Login.Api as Api exposing (LoginResponse(..))
import Pages.Login.Model exposing (Model)
import Pages.Login.Selectors as Selectors
import Pages.Login.Validation as Validation exposing (LoginField(..), LoginValidationError(..))
import RemoteData exposing (RemoteData(..), WebData)
import Route
import UI.Button as Button
import UI.Container as Container
import UI.Error as Error
import UI.Input as Input
import UI.Link as Link
import Utils.Styles exposing (StyledElement)
import Utils.Validation exposing (getErrorForField)


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
        ]


type alias Props =
    { usernameError : Maybe String
    , passwordError : Maybe String
    , usernameValue : String
    , passwordValue : String
    , isSubmitButtonDisabled : Bool
    , loginRequestErrorText : String
    }


view : Props -> Html Msg
view props =
    loginForm [ onSubmit LoginAttempted ]
        [ title [] [ text "Sign In" ]
        , Input.view
            { validationError = props.usernameError
            , inputAttributes =
                [ placeholder "Username"
                , value props.usernameValue
                , onInput LoginUsernameUpdated
                ]
            }
            []
            []
        , Input.view
            { validationError = props.passwordError
            , inputAttributes =
                [ placeholder "Password"
                , type_ "password"
                , value props.passwordValue
                , onInput LoginPasswordUpdated
                ]
            }
            []
            []
        , submitButton
            [ type_ "submit"
            , disabled props.isSubmitButtonDisabled
            ]
            [ text "Let's go!" ]
        , Error.text { error = props.loginRequestErrorText } [] []
        , Link.view { to = Route.Register } [] [ text "Don't have an account?" ]
        ]
