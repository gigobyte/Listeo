module Pages.Layout exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import UI.Header as Header
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled main_
        [ height <| pct 100
        ]


view : Html Msg -> AppModel -> Html Msg
view page model =
    case model.auth.fetchUserResponse of
        Loading ->
            text "Loading..."

        _ ->
            container []
                [ Header.view model
                , page
                ]
