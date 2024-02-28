module Page.Signin exposing (Model, Msg, OutMsg(..), init, update, view)

import Api
import Custom.Events exposing (onEnter)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Http
import Model.Session exposing (Session)
import Model.UserPassword exposing (..)
import Route



-- Types --


type alias Model =
    { session : Session
    , userPassword : UserPassword
    , signinError : Maybe String
    , wrongPassword : Bool
    }


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | Submit
    | Submitted (Result Http.Error String)


type OutMsg
    = ChangeToken String



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , userPassword = defaultUserPassword
      , signinError = Nothing
      , wrongPassword = False
      }
    , Cmd.none
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        UsernameChanged newName ->
            let
                rename oldUser =
                    { oldUser | username = newName }
            in
            ( { model | userPassword = rename model.userPassword }, Cmd.none, Nothing )

        PasswordChanged newPassword ->
            let
                repass oldUser =
                    { oldUser | password = newPassword }
            in
            ( { model | userPassword = repass model.userPassword }, Cmd.none, Nothing )

        Submit ->
            ( model, signinAttempt model.session.token model.userPassword, Nothing )

        Submitted (Ok "") ->
            ( { model | signinError = Nothing, wrongPassword = True }, Cmd.none, Nothing )

        Submitted (Ok token) ->
            ( model, Route.pushUrl model.session.navkey Route.Home, Just (ChangeToken token) )

        Submitted (Err err) ->
            ( { model | signinError = Just (buildErrorMessage err), wrongPassword = False }, Cmd.none, Nothing )



-- API Requests --


signinAttempt : Maybe String -> UserPassword -> Cmd Msg
signinAttempt token userPassword =
    Api.postRequest token
        Api.Signin
        (Http.jsonBody (userPasswordEncoder userPassword))
    <|
        Http.expectString Submitted



-- View --


view : Model -> Html Msg
view model =
    row []
        [ mainHeader [] [ text "Sign in" ]
        , viewSigninError model.signinError
        , viewWrongPassword model.wrongPassword
        , viewForm model
        ]


viewSigninError : Maybe String -> Html msg
viewSigninError maybeError =
    case maybeError of
        Just error ->
            errorText []
                [ emphasisText [] [ text "Couldn't sign in at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""


viewWrongPassword : Bool -> Html Msg
viewWrongPassword wrongPassword =
    if wrongPassword then
        errorText [] [ text "Incorrect Username or Password." ]

    else
        text ""


viewForm : Model -> Html Msg
viewForm model =
    inputForm []
        [ viewUsernameField model.userPassword
        , viewPasswordField model.userPassword
        , submitButton Submit [ text "Sign in" ]
        ]


viewUsernameField : UserPassword -> Html Msg
viewUsernameField userPassword =
    textInput
        [ onInput UsernameChanged
        , onEnter Submit
        , value userPassword.username
        ]
        [ text "Username" ]


viewPasswordField : UserPassword -> Html Msg
viewPasswordField userPassword =
    passwordInput
        [ onInput PasswordChanged
        , onEnter Submit
        , value userPassword.password
        ]
        [ text "Password" ]
