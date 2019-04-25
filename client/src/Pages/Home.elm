module Pages.Home exposing (Model, Msg, init, toSession, update, updateSession, view)

import Html.Styled exposing (div, text)
import Session exposing (Session)
import Utils.Styles exposing (StyledDocument)


type alias Model =
    { session : Session }


type Msg
    = NoOp


view : Model -> StyledDocument Msg
view _ =
    { title = "Home - Listeo"
    , body =
        [ div []
            [ text "Home page"
            ]
        ]
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- EXPORTS


toSession : Model -> Session
toSession model =
    model.session


updateSession : Session -> Model -> Model
updateSession newSession model =
    { model | session = newSession }
