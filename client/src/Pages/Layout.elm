module Pages.Layout exposing (view)

import Css exposing (..)
import Css.Global exposing (Snippet, global, typeSelector)
import Html.Styled exposing (..)
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Pages.AddPlaylist.AddPlaylistOverlay as AddPlaylistOverlay
import RemoteData exposing (RemoteData(..))
import UI.Colors exposing (whiteGray50)
import UI.Header as Header
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
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
        , backgroundColor whiteGray50
        ]
    ]


view : Html Msg -> AppModel -> Html Msg
view page model =
    case model.auth.fetchUserResponse of
        NotAsked ->
            text ""

        Loading ->
            text ""

        _ ->
            container []
                [ global globalStyle
                , Header.view model
                , case model.addPlaylist.isOverlayShown of
                    True ->
                        AddPlaylistOverlay.view model.addPlaylist

                    False ->
                        page
                ]
