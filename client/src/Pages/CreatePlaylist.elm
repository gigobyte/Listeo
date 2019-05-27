module Pages.CreatePlaylist exposing (Model, Msg, init, toSession, update, updateSession, view)

import Css exposing (..)
import Enum exposing (Enum)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import List.Extra as List
import RemoteData exposing (RemoteData(..))
import Result.Extra as Result
import Route
import Session exposing (Session)
import UI.Button as Button
import UI.Container as Container
import UI.Input as Input
import UI.RadioButton as RadioButton
import UI.TagInput as TagInput
import UI.Textarea as Textarea
import Utils.ErrorResponse exposing (ResponseData, expectJsonWithError)
import Utils.Fetch as Fetch exposing (ApiRoot, Token)
import Utils.Styles exposing (StyledDocument, StyledElement)
import Utils.Validation as Validation exposing (Problems(..), getErrorForField)
import Validate exposing (Valid, Validator, fromValid, ifBlank, validate)



-- MODEL


type alias Model =
    { session : Session
    , problems : Validation.Problems ValidatedField ValidationError
    , form : Form
    }


type alias Form =
    { playlistName : String
    , playlistDescription : String
    , playlistTagInput : String
    , playlistTags : List String
    , playlistPrivacy : PlaylistPrivacy
    , playlistStyle : PlaylistStyle
    }


type PlaylistPrivacy
    = Public
    | Private


playlistPrivacyEnum : Enum PlaylistPrivacy
playlistPrivacyEnum =
    Enum.create
        [ ( "Public", Public )
        , ( "Private", Private )
        ]


type PlaylistStyle
    = Unordered
    | Ranked


playlistStyleEnum : Enum PlaylistStyle
playlistStyleEnum =
    Enum.create
        [ ( "Unordered", Unordered )
        , ( "Ranked", Ranked )
        ]


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , problems = NotShown
      , form =
            { playlistName = ""
            , playlistDescription = ""
            , playlistTagInput = ""
            , playlistTags = []
            , playlistPrivacy = Public
            , playlistStyle = Unordered
            }
      }
    , Cmd.none
    )



-- VIEW


view : Model -> StyledDocument Msg
view model =
    { title = "Create playlist - Listeo"
    , body =
        [ viewContainer []
            [ viewTitle [] [ text "Create a new playlist" ]
            , Input.view
                { validationError = getErrorForField PlaylistName model.problems |> Maybe.map validationErrorToString
                , inputAttributes =
                    [ placeholder "Name of list"
                    , value model.form.playlistName
                    , onInput PlaylistNameUpdated
                    ]
                }
                []
                []
            , TagInput.view
                { onAddTag = PlaylistTagAdded
                , onRemoveTag = PlaylistTagRemoved
                , value = model.form.playlistTagInput
                , tags = model.form.playlistTags
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
                        { isChecked = model.form.playlistPrivacy == Public
                        , label = "Public"
                        , onCheck = PlaylistPrivacySelected Public
                        }
                        []
                        []
                    , viewSeparator [] []
                    , RadioButton.view
                        { isChecked = model.form.playlistStyle == Ranked
                        , label = "Ranked"
                        , onCheck = PlaylistStyleSelected Ranked
                        }
                        []
                        []
                    ]
                , viewSettingsColumn []
                    [ RadioButton.view
                        { isChecked = model.form.playlistPrivacy == Private
                        , label = "Private"
                        , onCheck = PlaylistPrivacySelected Private
                        }
                        []
                        []
                    , viewSeparator [] []
                    , RadioButton.view
                        { isChecked = model.form.playlistStyle == Unordered
                        , label = "Unordered"
                        , onCheck = PlaylistStyleSelected Unordered
                        }
                        []
                        []
                    ]
                ]
            , Button.view [ onClick CreatePlaylistAttempted ] [ text "Create" ]
            ]
        ]
    }


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



-- UPDATE


type Msg
    = PlaylistNameUpdated String
    | PlaylistTagInputUpdated String
    | PlaylistTagAdded String
    | PlaylistTagRemoved String
    | PlaylistPrivacySelected PlaylistPrivacy
    | PlaylistStyleSelected PlaylistStyle
    | CreatePlaylistAttempted
    | CreatePlaylist (ResponseData CreatePlaylistResponseError CreatePlaylistResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaylistNameUpdated value ->
            updateForm (\form -> { form | playlistName = value }) model

        PlaylistTagInputUpdated value ->
            if String.endsWith "," value then
                updateForm (\form -> { form | playlistTagInput = String.trim value }) model

            else
                update (PlaylistTagAdded <| String.dropRight 1 value) model

        PlaylistTagAdded tag ->
            updateForm
                (\form ->
                    { form
                        | playlistTags = List.unique (form.playlistTags ++ [ tag ])
                        , playlistTagInput = ""
                    }
                )
                model

        PlaylistTagRemoved tag ->
            updateForm (\form -> { form | playlistTags = List.remove tag form.playlistTags }) model

        PlaylistPrivacySelected option ->
            updateForm (\form -> { form | playlistPrivacy = option }) model

        PlaylistStyleSelected option ->
            updateForm (\form -> { form | playlistStyle = option }) model

        CreatePlaylistAttempted ->
            case validate validator model.form of
                Ok validForm ->
                    ( model, createPlaylist model.session.apiRoot model.session.token validForm |> Cmd.map CreatePlaylist )

                Err problems ->
                    ( { model | problems = Shown problems }, Cmd.none )

        CreatePlaylist (Success { playlistId }) ->
            ( model, Route.pushUrl model.session.navKey (Route.ViewPlaylist playlistId) )

        CreatePlaylist _ ->
            ( model, Cmd.none )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    let
        newForm =
            transform model.form
    in
    ( { model
        | form = newForm
        , problems =
            Validation.mapProblems
                (\_ ->
                    validate validator newForm
                        |> Result.map (always [])
                        |> Result.merge
                )
                model.problems
      }
    , Cmd.none
    )



-- FORM


type ValidatedField
    = PlaylistName


type ValidationError
    = PlaylistNameMissing


validator : Validator ( ValidatedField, ValidationError ) Form
validator =
    Validate.all
        [ ifBlank .playlistName ( PlaylistName, PlaylistNameMissing )
        ]


validationErrorToString : ValidationError -> String
validationErrorToString error =
    case error of
        PlaylistNameMissing ->
            "Please enter the name of the playlist"



-- API


createPlaylist : ApiRoot -> Token -> Valid Form -> Cmd (ResponseData CreatePlaylistResponseError CreatePlaylistResponse)
createPlaylist apiRoot token form =
    let
        formValues =
            fromValid form

        body =
            Encode.object
                [ ( "name", Encode.string formValues.playlistName )
                , ( "description", Encode.string formValues.playlistDescription )
                , ( "tags", Encode.list Encode.string formValues.playlistTags )
                , ( "privacy", playlistPrivacyEnum.encode formValues.playlistPrivacy )
                , ( "style", playlistStyleEnum.encode formValues.playlistStyle )
                ]
    in
    Fetch.postWithAuth
        { url = Fetch.createPlaylist apiRoot
        , token = token
        , expect =
            expectJsonWithError createPlaylistResponseErrorEnum.decoder createPlaylistResponseDecoder
        , body = body |> Http.jsonBody
        }


type CreatePlaylistResponseError
    = InvalidRequest
    | ValidationFailed


createPlaylistResponseErrorEnum : Enum CreatePlaylistResponseError
createPlaylistResponseErrorEnum =
    Enum.create
        [ ( "InvalidRequest", InvalidRequest )
        , ( "ValidationFailed", ValidationFailed )
        ]


type alias CreatePlaylistResponse =
    { playlistId : String }


createPlaylistResponseDecoder : Decoder CreatePlaylistResponse
createPlaylistResponseDecoder =
    Decode.succeed CreatePlaylistResponse
        |> required "playlistId" string



-- EXPORTS


toSession : Model -> Session
toSession model =
    model.session


updateSession : Session -> Model -> Model
updateSession newSession model =
    { model | session = newSession }
