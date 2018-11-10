module Pages.Layout exposing (view)

import Model exposing (AppModel)
import Html.Styled exposing (..)
import UI.Header as Header
import Utils.Styles exposing (StyledElement)
import Css exposing (..)

container : StyledElement msg
container = 
    styled main_ [
        height <| pct 100
    ]

view : Html msg -> AppModel -> Html msg
view page model =
    container [] [
        Header.view,
        page
    ]