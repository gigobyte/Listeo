module Pages.CreatePlaylist.Validation exposing
    ( createPlaylistValidator
    , makeCreatePlaylistRequestModel
    )

import Pages.CreatePlaylist.Api exposing (CreatePlaylistRequest)
import Pages.CreatePlaylist.Model exposing (Model)
import UI.TagInput exposing (tagValue)
import Validate exposing (Validator, fromValid, ifBlank, validate)


type CreatePlaylistValidationError
    = PlaylistNameMissing


type CreatePlaylistField
    = PlaylistName


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
                , tags = List.map tagValue validatedModel.playlistTags
                , privacy = validatedModel.playlistPrivacy
                , style = validatedModel.playlistStyle
                }
            )
        |> Result.toMaybe
