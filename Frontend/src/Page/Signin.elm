module Page.Signin exposing (Model, Msg, OutMsg(..), init, update, view)

import Api
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    div []
        [ h3 [] [ text "Sign in" ]
        , br [] []
        , viewSigninError model.signinError
        , viewWrongPassword model.wrongPassword
        , viewForm model
        ]


viewSigninError : Maybe String -> Html msg
viewSigninError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't sign in at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


viewWrongPassword : Bool -> Html Msg
viewWrongPassword wrongPassword =
    if wrongPassword then
        div [ Custom.Attributes.errorMessage ]
            [ text "Incorrect Username or Password."
            ]

    else
        text ""


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ viewUsernameField model.userPassword
        , viewPasswordField model.userPassword
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Sign in" ]
        ]


viewUsernameField : UserPassword -> Html Msg
viewUsernameField userPassword =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "usernameInput")
            [ text "User Name" ]
        , input
            (Custom.Attributes.formInput "usernameInput"
                [ onInput UsernameChanged
                , onEnter Submit
                , value userPassword.username
                ]
            )
            []
        ]


viewPasswordField : UserPassword -> Html Msg
viewPasswordField userPassword =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "passwordInput")
            [ text "Password" ]
        , input
            (Custom.Attributes.formPasswordInput "passwordInput"
                [ onInput PasswordChanged
                , onEnter Submit
                , value userPassword.password
                ]
            )
            []
        ]
