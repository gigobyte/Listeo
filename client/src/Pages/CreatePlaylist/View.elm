module Pages.CreatePlaylist.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, placeholder, type_, value)
import Html.Styled.Events exposing (onInput)
import Msg exposing (Msg(..))
import Pages.CreatePlaylist.Model exposing (Model)
import UI.Container as Container
import UI.Input as Input
import UI.TagInput as TagInput
import UI.Textarea as Textarea
import Utils.Styles exposing (StyledElement)


container : StyledElement msg
container =
    styled (Container.centered div)
        [ height <| pct 66
        ]


title : StyledElement msg
title =
    styled h1
        [ fontSize <| rem 2
        ]


type alias Props =
    { playlistNameValue : String
    , playlistNameError : Maybe String
    , playlistTagInput : String
    , playlistTags : List TagInput.Tag
    }


view : Props -> Html Msg
view props =
    container []
        [ title [] [ text "Create a new playlist" ]
        , Input.view
            { validationError = props.playlistNameError
            , inputAttributes =
                [ placeholder "Name of list"
                , value props.playlistNameValue
                , onInput PlaylistNameUpdated
                ]
            }
            []
            []
        , TagInput.view
            { onAddTag = PlaylistTagAdded
            , onRemoveTag = PlaylistTagRemoved
            , value = props.playlistTagInput
            , tags = props.playlistTags
            , inputAttributes =
                [ placeholder "Tags (optional)"
                , onInput PlaylistTagInputUpdated
                ]
            }
            []
            []
        , Textarea.view
            { validationError = Nothing
            , textareaAttributes = [ placeholder "Description" ]
            }
            []
            []
        ]
