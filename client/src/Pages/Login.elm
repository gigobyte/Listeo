port module Pages.Login exposing (Model, Msg(..), init, toSession, update, updateSession, view)

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
import Utils.Validation exposing (getErrorForField)
import Validate exposing (Valid, Validator, fromValid, ifBlank, validate)



-- MODEL


type alias Model =
    { session : Session
    , problems : List ( ValidatedField, ValidationError )
    , form : Form
    , loginResponse : ResponseData LoginResponseError LoginResponse
    }


type alias Form =
    { username : String
    , password : String
    }


type LoginResponseError
    = UserNotFound
    | InvalidRequest
    | ServerError


type alias LoginResponse =
    { jwt : String }


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , problems = []
      , form =
            { username = ""
            , password = ""
            }
      , loginResponse = NotAsked
      }
    , Cmd.none
    )



-- VIEW


view : Model -> StyledDocument Msg
view model =
    let
        loginRequestErrorText =
            case model.loginResponse of
                Failure (BadResponse error) ->
                    loginErrorToString error

                _ ->
                    ""
    in
    { title = "Login - Listeo"
    , body =
        [ viewLoginForm [ onSubmit LoginAttempted ]
            [ viewTitle [] [ text "Sign In" ]
            , Input.view
                { validationError = getErrorForField Username model.problems |> Maybe.map validationErrorToString
                , inputAttributes =
                    [ placeholder "Username"
                    , value model.form.username
                    , onInput EnteredUsername
                    ]
                }
                []
                []
            , Input.view
                { validationError = getErrorForField Password model.problems |> Maybe.map validationErrorToString
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
                [ text "Let's go!" ]
            , Error.viewText { error = loginRequestErrorText } [] []
            , Link.view
                { to = Route.Register
                }
                []
                [ text "Don't have an account?" ]
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
    | LoginAttempted
    | CompletedLogin (ResponseData LoginResponseError LoginResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredUsername value ->
            updateForm (\form -> { form | username = String.trim value }) model

        EnteredPassword value ->
            updateForm (\form -> { form | password = String.trim value }) model

        LoginAttempted ->
            case validate validator model.form of
                Ok validForm ->
                    ( model, login model.session.apiRoot validForm |> Cmd.map CompletedLogin )

                Err problems ->
                    ( { model | problems = problems }, Cmd.none )

        CompletedLogin ((Success { jwt }) as response) ->
            let
                token =
                    Token (Just jwt)
            in
            ( { model
                | loginResponse = response
              }
            , storeJwt jwt
            )

        CompletedLogin response ->
            ( { model | loginResponse = response }, Cmd.none )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



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


loginErrorToString : LoginResponseError -> String
loginErrorToString err =
    case err of
        UserNotFound ->
            "User not found"

        _ ->
            "Something went wrong"


isSubmitButtonDisabled : Model -> Bool
isSubmitButtonDisabled model =
    isLoading model.loginResponse || (List.length model.problems > 0)



-- API


loginResponseErrorEnum : Enum LoginResponseError
loginResponseErrorEnum =
    Enum.create
        [ ( "UserNotFound", UserNotFound )
        , ( "InvalidRequest", InvalidRequest )
        , ( "ServerError", ServerError )
        ]


loginResponseDecoder : Decoder LoginResponse
loginResponseDecoder =
    Decode.succeed LoginResponse
        |> required "jwt" Decode.string


login : ApiRoot -> Valid Form -> Cmd (ResponseData LoginResponseError LoginResponse)
login apiRoot form =
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
        { url = Fetch.login apiRoot
        , expect =
            expectJsonWithError loginResponseErrorEnum.decoder loginResponseDecoder
        , body = body |> Http.jsonBody
        }



-- EXPORTS


toSession : Model -> Session
toSession model =
    model.session


updateSession : Session -> Model -> Model
updateSession newSession model =
    { model | session = newSession }
