module Page.AddCoach exposing (Model, Msg, init, update, view)

import Api
import Custom.Events exposing (onEnter)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Http
import Model.Coach exposing (Coach, coachDecoder, defaultCoach, newCoachEncoder)
import Model.Session exposing (Session)



-- Types --


type alias Model =
    { session : Session
    , coach : Coach
    , submitError : Maybe String
    }


type Msg
    = NameChanged String
    | Submit
    | CoachSubmitted (Result Http.Error Coach)



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , coach = defaultCoach
      , submitError = Nothing
      }
    , Cmd.none
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChanged newName ->
            let
                rename oldCoach =
                    { oldCoach | name = newName }
            in
            ( { model | coach = rename model.coach }, Cmd.none )

        Submit ->
            ( model, submitCoach model.session.token model.coach )

        CoachSubmitted (Ok _) ->
            ( { model | coach = defaultCoach, submitError = Nothing }, Cmd.none )

        CoachSubmitted (Err err) ->
            ( { model | submitError = Just (buildErrorMessage err) }, Cmd.none )



-- API Requests --


submitCoach : Maybe String -> Coach -> Cmd Msg
submitCoach token coach =
    Api.postRequest token
        Api.Coaches
        (Http.jsonBody (newCoachEncoder coach))
    <|
        Http.expectJson CoachSubmitted coachDecoder



-- View --


view : Model -> Html Msg
view model =
    row []
        [ mainHeader [] [ text "Add Coach" ]
        , viewError model.submitError
        , viewForm model.coach
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            errorText []
                [ emphasisText [] [ text "Couldn't save a coach at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""


viewForm : Coach -> Html Msg
viewForm coach =
    inputForm []
        [ textInput
            [ onInput NameChanged
            , onEnter Submit
            , value coach.name
            ]
            [ text "Name" ]
        , submitButton Submit [ text "Add" ]
        ]
