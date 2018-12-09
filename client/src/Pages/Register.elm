module Pages.Register exposing (init, title, update, view)

import Html.Styled exposing (Html)
import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Register.Model exposing (Model)
import Pages.Register.Selectors as Selectors
import Pages.Register.Update as Update
import Pages.Register.View as View
import Selectors


view : AppModel -> Html Msg
view model =
    View.view
        { passwordError = Selectors.getPasswordError model
        , passwordValue = Selectors.getPasswordValue model
        , usernameError = Selectors.getUsernameError model
        , usernameValue = Selectors.getUsernameValue model
        , registerRequestErrorText = Selectors.getRegisterRequestErrorText model
        , isSubmitButtonDisabled = Selectors.isSubmitButtonDisabled model
        }


update : Msg -> AppModel -> ( Model, Cmd Msg )
update msg model =
    Update.update msg
        model.register
        { key = Selectors.getNavKey model
        , route = Selectors.getRoute model
        }


init =
    Update.init


title : AppModel -> String
title _ =
    "Register - Listeo"
