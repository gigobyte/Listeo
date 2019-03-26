module Pages.Layout exposing (view)

import Css exposing (..)
import Css.Global exposing (Snippet, global, typeSelector)
import Html.Styled exposing (..)
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Pages.Header as Header
import Pages.Header.AddPlaylistModal as AddPlaylistModal
import RemoteData exposing (RemoteData(..))
import UI.Colors exposing (gray200)
import Utils.Styles exposing (StyledElement)


viewContainer : StyledElement msg
viewContainer =
    styled main_
        [ height <| pct 100
        , flexDirection column
        , displayFlex
        ]


globalStyle : List Snippet
globalStyle =
    [ typeSelector "html, body"
        [ height <| pct 100
        , margin zero
        , fontFamilies [ "Museo-Sans" ]
        , backgroundColor gray200
        ]
    ]


view : Html Msg -> AppModel -> Html Msg
view page model =
    case model.auth.user of
        NotAsked ->
            text ""

        Loading ->
            text ""

        _ ->
            viewContainer []
                [ global globalStyle
                , Header.view model
                , page
                , case model.header.isOverlayShown of
                    True ->
                        AddPlaylistModal.view

                    False ->
                        text ""
                ]
