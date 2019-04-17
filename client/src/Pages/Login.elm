module Pages.Login exposing (init, update, view)

import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Login.Model exposing (Model)
import Pages.Login.Selectors as Selectors
import Pages.Login.Update as Update
import Pages.Login.View as View
import Session exposing (Session)
import Utils.Styles exposing (StyledDocument)


view : AppModel -> StyledDocument Msg
view model =
    { title = "Login - Listeo"
    , body =
        [ View.view
            { passwordError = Selectors.getPasswordError model.login
            , passwordValue = Selectors.getPasswordValue model.login
            , usernameError = Selectors.getUsernameError model.login
            , usernameValue = Selectors.getUsernameValue model.login
            , loginRequestErrorText = Selectors.getLoginRequestErrorText model.login
            , isSubmitButtonDisabled = Selectors.isSubmitButtonDisabled model.login
            }
        ]
    }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Update.update msg model.login session


init : Model
init =
    Update.init
