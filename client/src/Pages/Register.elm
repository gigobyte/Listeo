module Pages.Register exposing (init, update, view)

import Model exposing (AppModel)
import Msg exposing (Msg)
import Pages.Register.Model exposing (Model)
import Pages.Register.Selectors as Selectors
import Pages.Register.Update as Update
import Pages.Register.View as View
import Session exposing (Session)
import Utils.Styles exposing (StyledDocument)


view : AppModel -> StyledDocument Msg
view model =
    { title = "Register - Listeo"
    , body =
        [ View.view
            { passwordError = Selectors.getPasswordError model.register
            , passwordValue = Selectors.getPasswordValue model.register
            , usernameError = Selectors.getUsernameError model.register
            , usernameValue = Selectors.getUsernameValue model.register
            , registerRequestErrorText = Selectors.getRegisterRequestErrorText model.register
            , isSubmitButtonDisabled = Selectors.isSubmitButtonDisabled model.register
            }
        ]
    }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Update.update msg model.register session


init : Model
init =
    Update.init
