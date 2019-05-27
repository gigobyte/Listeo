port module Pages.Register exposing (Model, Msg(..), init, toSession, update, updateSession, view)

import Css exposing (..)
import Enum exposing (Enum)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, placeholder, type_, value)
import Html.Styled.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), isLoading)
import Result.Extra as Result
import Route
import Session exposing (Msg(..), Session, storeJwt)
import UI.Button as Button
import UI.Container as Container
import UI.Error as Error
import UI.Input as Input
import UI.Link as Link
import Utils.ErrorResponse exposing (HttpError(..), ResponseData, expectJsonWithError)
import Utils.Fetch as Fetch exposing (ApiRoot, Token(..))
import Utils.Styles exposing (StyledDocument, StyledElement)
import Utils.Validation as Validation exposing (Problems(..))
import Validate exposing (Valid, Validator, fromValid, ifBlank, validate)



-- MODEL


type alias Model =
    { session : Session
    , problems : Validation.Problems ValidatedField ValidationError
    , form : Form
    , registerResponse : ResponseData RegisterResponseError RegisterResponse
    }


type alias RegisterResponse =
    { jwt : String }


type alias Form =
    { username : String
    , password : String
    }


type RegisterResponseError
    = UserAlreadyExists
    | InvalidRequest
    | PasswordHashingFailed
    | ValidationFailed


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , problems = NotShown
      , form =
            { username = ""
            , password = ""
            }
      , registerResponse = NotAsked
      }
    , Cmd.none
    )



-- VIEW


view : Model -> StyledDocument Msg
view model =
    let
        registerRequestErrorText =
            case model.registerResponse of
                Failure (BadResponse error) ->
                    registerErrorToString error

                _ ->
                    ""
    in
    { title = "Register - Listeo"
    , body =
        [ viewLoginForm [ onSubmit RegisterAttempted ]
            [ viewTitle [] [ text "Register" ]
            , Input.view
                { validationError = Validation.getErrorForField Username model.problems |> Maybe.map validationErrorToString
                , inputAttributes =
                    [ placeholder "Username"
                    , value model.form.username
                    , onInput EnteredUsername
                    ]
                }
                []
                []
            , Input.view
                { validationError = Validation.getErrorForField Password model.problems |> Maybe.map validationErrorToString
                , inputAttributes =
                    [ placeholder "Password"
                    , type_ "password"
                    , value model.form.password
                    , onInput EnteredPassword
                    ]
                }
                []
                []
            , viewSubmitButton
                [ type_ "submit"
                , disabled (isSubmitButtonDisabled model)
                ]
                [ text "Beam me up!" ]
            , Error.viewText { error = registerRequestErrorText } [] []
            , Link.view { to = Route.Login } [] [ text "Already have an account?" ]
            ]
        ]
    }


viewLoginForm : StyledElement msg
viewLoginForm =
    styled (Container.viewCentered form)
        [ height <| pct 66
        ]


viewTitle : StyledElement msg
viewTitle =
    styled h1
        [ fontSize <| rem 3
        ]


viewSubmitButton : StyledElement msg
viewSubmitButton =
    styled Button.view
        [ marginTop <| px 10
        , marginBottom <| px 15
        ]



-- UPDATE


type Msg
    = EnteredUsername String
    | EnteredPassword String
    | RegisterAttempted
    | CompletedRegistration (ResponseData RegisterResponseError RegisterResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredUsername value ->
            updateForm (\form -> { form | username = String.trim value }) model

        EnteredPassword value ->
            updateForm (\form -> { form | password = String.trim value }) model

        RegisterAttempted ->
            case validate validator model.form of
                Ok validForm ->
                    ( model, register model.session.apiRoot validForm |> Cmd.map CompletedRegistration )

                Err problems ->
                    ( { model | problems = Shown problems }, Cmd.none )

        CompletedRegistration ((Success { jwt }) as response) ->
            ( { model | registerResponse = response }
            , storeJwt jwt
            )

        CompletedRegistration response ->
            ( { model | registerResponse = response }, Cmd.none )


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
    = Username
    | Password


type ValidationError
    = UsernameMissing
    | PasswordMissing


validator : Validator ( ValidatedField, ValidationError ) Form
validator =
    Validate.all
        [ ifBlank .username ( Username, UsernameMissing )
        , ifBlank .password ( Password, PasswordMissing )
        ]


validationErrorToString : ValidationError -> String
validationErrorToString error =
    case error of
        UsernameMissing ->
            "Please enter username"

        PasswordMissing ->
            "Please enter password"


registerErrorToString : RegisterResponseError -> String
registerErrorToString err =
    case err of
        UserAlreadyExists ->
            "User already exists"

        _ ->
            "Something went wrong"


isSubmitButtonDisabled : Model -> Bool
isSubmitButtonDisabled model =
    isLoading model.registerResponse || Validation.hasFailed model.problems



-- API


registerResponseErrorEnum : Enum RegisterResponseError
registerResponseErrorEnum =
    Enum.create
        [ ( "UserAlreadyExists", UserAlreadyExists )
        , ( "InvalidRequest", InvalidRequest )
        , ( "PasswordHashingFailed", PasswordHashingFailed )
        , ( "ValidationFailed", ValidationFailed )
        ]


registerResponseDecoder : Decoder RegisterResponse
registerResponseDecoder =
    Decode.succeed RegisterResponse
        |> required "jwt" Decode.string


register : ApiRoot -> Valid Form -> Cmd (ResponseData RegisterResponseError RegisterResponse)
register apiRoot form =
    let
        formValues =
            fromValid form

        body =
            Encode.object
                [ ( "username", Encode.string formValues.username )
                , ( "password", Encode.string formValues.password )
                ]
    in
    Fetch.post
        { url = Fetch.register apiRoot
        , expect =
            expectJsonWithError registerResponseErrorEnum.decoder registerResponseDecoder
        , body = body |> Http.jsonBody
        }



-- EXPORTS


toSession : Model -> Session
toSession model =
    model.session


updateSession : Session -> Model -> Model
updateSession newSession model =
    { model | session = newSession }
