module Pages.Login exposing (init, update, view)

import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Login.Model exposing (Model)
import Pages.Login.Selectors as Selector
import Pages.Login.Update as Login
import Pages.Login.View as Login
import Session exposing (Session)
import Utils.Styles exposing (StyledDocument)


view : AppModel -> StyledDocument Msg
view model =
    { title = "Login - Listeo"
    , body =
        [ Login.view
            { passwordError = Selector.getPasswordError model.login
            , passwordValue = Selector.getPasswordValue model.login
            , usernameError = Selector.getUsernameError model.login
            , usernameValue = Selector.getUsernameValue model.login
            , loginRequestErrorText = Selector.getLoginRequestErrorText model.login
            , isSubmitButtonDisabled = Selector.isSubmitButtonDisabled model.login
            }
        ]
    }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Login.update msg model.login session


init : Model
init =
    Login.init
