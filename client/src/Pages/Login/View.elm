module Pages.Login.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Msg exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Route
import UI.Button as Button
import UI.Container as Container
import UI.Error as Error
import UI.Input as Input
import UI.Link as Link
import Utils.Styles exposing (StyledElement)


viewLoginForm : StyledElement msg
viewLoginForm =
    styled (Container.viewCentered form)
        [ height <| pct 66
        ]


viewTitle : StyledElement msg
viewTitle =
    styled h1
        [ fontSize <| rem 3
        ]


viewSubmitButton : StyledElement msg
viewSubmitButton =
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
    viewLoginForm [ onSubmit LoginAttempted ]
        [ viewTitle [] [ text "Sign In" ]
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
        , viewSubmitButton
            [ type_ "submit"
            , disabled props.isSubmitButtonDisabled
            ]
            [ text "Let's go!" ]
        , Error.viewText { error = props.loginRequestErrorText } [] []
        , Link.view { to = Route.Register } [] [ text "Don't have an account?" ]
        ]
