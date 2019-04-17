module Pages.Register exposing (init, update, view)

import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Register.Model exposing (Model)
import Pages.Register.Selectors as Selector
import Pages.Register.Update as Register
import Pages.Register.View as Register
import Session exposing (Session)
import Utils.Styles exposing (StyledDocument)


view : AppModel -> StyledDocument Msg
view model =
    { title = "Register - Listeo"
    , body =
        [ Register.view
            { passwordError = Selector.getPasswordError model.register
            , passwordValue = Selector.getPasswordValue model.register
            , usernameError = Selector.getUsernameError model.register
            , usernameValue = Selector.getUsernameValue model.register
            , registerRequestErrorText = Selector.getRegisterRequestErrorText model.register
            , isSubmitButtonDisabled = Selector.isSubmitButtonDisabled model.register
            }
        ]
    }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Register.update msg model.register session


init : Model
init =
    Register.init
