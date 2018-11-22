module Pages.Layout exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Model exposing (AppModel)
import UI.Header as Header
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled main_
        [ height <| pct 100
        ]


view : Html msg -> AppModel -> Html msg
view page model =
    container []
        [ Header.view model
        , page
        ]
