module Pages.Login exposing (Model, Msg(..), init, update, view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Types exposing (StyledElement)
import UI.Button as Button
import UI.Input as Input


type alias Model =
    { username : String
    , password : String
    }


type Msg
    = UsernameUpdated String
    | PasswordUpdated String
    | LoginAttempted


init : Model
init =
    { username = ""
    , password = ""
    }


update : Model -> Msg -> Model
update model msg =
    case msg of
        UsernameUpdated value ->
            { model | username = value }

        PasswordUpdated value ->
            { model | password = value }

        LoginAttempted ->
            Debug.todo ""



-- case getLoginRequestModel model of
--     Just m ->
--         Debug.todo ""
--     Nothing ->
--         Debug.todo ""


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
        , loginInput [ type_ "text", placeholder "Username", value model.username, onInput UsernameUpdated ] []
        , loginInput [ type_ "text", placeholder "Password", value model.password, onInput PasswordUpdated ] []
        , submitButton [ type_ "submit", onClick LoginAttempted ] [ text "Let's go!" ]
        ]
