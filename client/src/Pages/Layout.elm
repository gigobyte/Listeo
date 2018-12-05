module Pages.Layout exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Model exposing (AppModel)
import Msg exposing (Msg(..))
import Pages.AddPlaylist.AddPlaylistOverlay as AddPlaylistOverlay
import RemoteData exposing (RemoteData(..))
import UI.Header as Header
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled main_
        [ height <| pct 100
        , flexDirection column
        , displayFlex
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
                [ Header.view model
                , case model.addPlaylist.isOverlayShown of
                    True ->
                        AddPlaylistOverlay.view model.addPlaylist

                    False ->
                        page
                ]
