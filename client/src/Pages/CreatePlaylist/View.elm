module Pages.CreatePlaylist.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Msg exposing (Msg(..))
import Pages.CreatePlaylist.Model exposing (PlaylistPrivacy(..), PlaylistStyle(..))
import UI.Button as Button
import UI.Container as Container
import UI.Input as Input
import UI.RadioButton as RadioButton
import UI.TagInput as TagInput
import UI.Textarea as Textarea
import Utils.Styles exposing (StyledElement)


viewContainer : StyledElement msg
viewContainer =
    styled (Container.viewCentered div)
        [ height <| pct 66
        ]


viewTitle : StyledElement msg
viewTitle =
    styled h1
        [ fontSize <| rem 2
        ]


viewSettingsColumn : StyledElement msg
viewSettingsColumn =
    styled div
        [ displayFlex
        , flexDirection column
        , paddingRight <| px 15
        , lastChild
            [ paddingRight zero
            ]
        ]


viewSettingLabel : StyledElement msg
viewSettingLabel =
    styled span
        [ fontWeight bold
        ]


viewSettings : StyledElement msg
viewSettings =
    styled div
        [ displayFlex
        , padding2 (px 10) zero
        ]


viewSeparator : StyledElement msg
viewSeparator =
    styled div
        [ height <| px 5
        ]


type alias Props =
    { playlistNameValue : String
    , playlistNameError : Maybe String
    , playlistTagInput : String
    , playlistTags : List String
    , playlistPrivacy : PlaylistPrivacy
    , playlistStyle : PlaylistStyle
    }


view : Props -> Html Msg
view props =
    viewContainer []
        [ viewTitle [] [ text "Create a new playlist" ]
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
            , textareaAttributes = [ placeholder "Description (optional)" ]
            }
            []
            []
        , viewSettings []
            [ viewSettingsColumn []
                [ viewSettingLabel [] [ text "Privacy" ]
                , viewSeparator [] []
                , viewSettingLabel [] [ text "Style" ]
                ]
            , viewSettingsColumn []
                [ RadioButton.view
                    { isChecked = props.playlistPrivacy == Public
                    , label = "Public"
                    , onCheck = PlaylistPrivacySelected Public
                    }
                    []
                    []
                , viewSeparator [] []
                , RadioButton.view
                    { isChecked = props.playlistStyle == Ranked
                    , label = "Ranked"
                    , onCheck = PlaylistStyleSelected Ranked
                    }
                    []
                    []
                ]
            , viewSettingsColumn []
                [ RadioButton.view
                    { isChecked = props.playlistPrivacy == Private
                    , label = "Private"
                    , onCheck = PlaylistPrivacySelected Private
                    }
                    []
                    []
                , viewSeparator [] []
                , RadioButton.view
                    { isChecked = props.playlistStyle == Unordered
                    , label = "Unordered"
                    , onCheck = PlaylistStyleSelected Unordered
                    }
                    []
                    []
                ]
            ]
        , Button.view [ onClick CreatePlaylistAttempted ] [ text "Create" ]
        ]
