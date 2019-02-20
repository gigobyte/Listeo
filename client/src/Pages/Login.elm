module Pages.Login exposing (init, title, update, view)

import Env exposing (Env)
import Html.Styled exposing (Html)
import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Login.Model exposing (Model)
import Pages.Login.Selectors as Selectors
import Pages.Login.Update as Update
import Pages.Login.View as View


view : AppModel -> Html Msg
view model =
    View.view
        { passwordError = Selectors.getPasswordError model
        , passwordValue = Selectors.getPasswordValue model
        , usernameError = Selectors.getUsernameError model
        , usernameValue = Selectors.getUsernameValue model
        , loginRequestErrorText = Selectors.getLoginRequestErrorText model
        , isSubmitButtonDisabled = Selectors.isSubmitButtonDisabled model
        }


update : Msg -> AppModel -> Env -> ( Model, Cmd Msg )
update msg model env =
    Update.update msg model.login env


init : Model
init =
    Update.init


title : AppModel -> String
title _ =
    "Login - Listeo"
