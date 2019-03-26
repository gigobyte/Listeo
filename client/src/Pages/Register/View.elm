module Pages.Register.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Msg exposing (Msg(..))
import Route
import UI.Button as Button
import UI.Container as Container
import UI.Error as Error
import UI.Input as Input
import UI.Link as Link
import Utils.Styles exposing (StyledElement)


viewRegisterForm : StyledElement msg
viewRegisterForm =
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
    , usernameValue : String
    , passwordError : Maybe String
    , passwordValue : String
    , isSubmitButtonDisabled : Bool
    , registerRequestErrorText : String
    }


view : Props -> Html Msg
view props =
    viewRegisterForm [ onSubmit RegisterAttempted ]
        [ viewTitle [] [ text "Register" ]
        , Input.view
            { validationError = props.usernameError
            , inputAttributes =
                [ placeholder "Username"
                , value props.usernameValue
                , onInput RegisterUsernameUpdated
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
                , onInput RegisterPasswordUpdated
                ]
            }
            []
            []
        , viewSubmitButton
            [ type_ "submit"
            , disabled props.isSubmitButtonDisabled
            ]
            [ text "Beam me up!" ]
        , Error.viewText { error = props.registerRequestErrorText } [] []
        , Link.view { to = Route.Login } [] [ text "Already have an account?" ]
        ]
