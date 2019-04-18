module Pages.CreatePlaylist.Selectors exposing
    ( getPlaylistName
    , getPlaylistNameError
    , getPlaylistStyle
    , getPlaylistTagInputValue
    , getPlaylistTags
    , getPrivacyOption
    )

import Pages.CreatePlaylist.Model as CreatePlaylist exposing (PlaylistPrivacy, PlaylistStyle)
import Pages.CreatePlaylist.Validation as Validation exposing (CreatePlaylistField(..), CreatePlaylistValidationError, createPlaylistValidator)
import Result.Extra as Result
import Utils.Validation exposing (getErrorForField)
import Validate exposing (validate)


getValidationErrors : CreatePlaylist.Model -> List ( CreatePlaylistField, CreatePlaylistValidationError )
getValidationErrors model =
    if model.showErrors then
        validate createPlaylistValidator model
            |> Result.map (always [])
            |> Result.merge

    else
        []


getPlaylistName : CreatePlaylist.Model -> String
getPlaylistName model =
    model.playlistName


getPlaylistTagInputValue : CreatePlaylist.Model -> String
getPlaylistTagInputValue model =
    model.playlistTagInput


getPlaylistTags : CreatePlaylist.Model -> List String
getPlaylistTags model =
    model.playlistTags


getPrivacyOption : CreatePlaylist.Model -> PlaylistPrivacy
getPrivacyOption model =
    model.playlistPrivacy


getPlaylistStyle : CreatePlaylist.Model -> PlaylistStyle
getPlaylistStyle model =
    model.playlistStyle


getPlaylistNameError : CreatePlaylist.Model -> Maybe String
getPlaylistNameError model =
    getErrorForField PlaylistName (getValidationErrors model)
        |> Maybe.map Validation.errToString
