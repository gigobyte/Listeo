module Pages.Register exposing (init, update, view)

import Html.Styled exposing (Html)
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
            { passwordError = Selectors.getPasswordError model
            , passwordValue = Selectors.getPasswordValue model
            , usernameError = Selectors.getUsernameError model
            , usernameValue = Selectors.getUsernameValue model
            , registerRequestErrorText = Selectors.getRegisterRequestErrorText model
            , isSubmitButtonDisabled = Selectors.isSubmitButtonDisabled model
            }
        ]
    }


update : Msg -> AppModel -> Session -> ( Model, Cmd Msg )
update msg model session =
    Update.update msg model.register session


init : Model
init =
    Update.init
