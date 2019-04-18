module Pages.CreatePlaylist.Validation exposing
    ( CreatePlaylistField(..)
    , CreatePlaylistValidationError(..)
    , createPlaylistValidator
    , errToString
    , makeCreatePlaylistRequestModel
    )

import Pages.CreatePlaylist.Api exposing (CreatePlaylistRequest)
import Pages.CreatePlaylist.Model exposing (Model)
import Validate exposing (Validator, fromValid, ifBlank, validate)


type CreatePlaylistValidationError
    = PlaylistNameMissing


type CreatePlaylistField
    = PlaylistName


errToString : CreatePlaylistValidationError -> String
errToString error =
    case error of
        PlaylistNameMissing ->
            "Please enter the name of the playlist"


createPlaylistValidator : Validator ( CreatePlaylistField, CreatePlaylistValidationError ) Model
createPlaylistValidator =
    Validate.all
        [ ifBlank .playlistName ( PlaylistName, PlaylistNameMissing )
        ]


makeCreatePlaylistRequestModel : Model -> Maybe CreatePlaylistRequest
makeCreatePlaylistRequestModel model =
    validate createPlaylistValidator model
        |> Result.map fromValid
        |> Result.map
            (\validatedModel ->
                { name = validatedModel.playlistName
                , description = validatedModel.playlistDescription
                , tags = validatedModel.playlistTags
                , privacy = validatedModel.playlistPrivacy
                , style = validatedModel.playlistStyle
                }
            )
        |> Result.toMaybe
