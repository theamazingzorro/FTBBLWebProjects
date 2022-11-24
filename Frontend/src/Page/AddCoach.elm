module Page.AddCoach exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    Api.postRequest token Api.Coaches
        (Http.jsonBody (newCoachEncoder coach))
    <|
        Http.expectJson CoachSubmitted coachDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Add Coach" ]
        , br [] []
        , viewError model.submitError
        , viewForm model.coach
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a coach at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


viewForm : Coach -> Html Msg
viewForm coach =
    div []
        [ div [ Custom.Attributes.formEntry ]
            [ label
                (Custom.Attributes.formLabel "nameInput")
                [ text "Name" ]
            , input
                (Custom.Attributes.formInput "nameInput"
                    [ onInput NameChanged
                    , onEnter Submit
                    , value coach.name
                    ]
                )
                []
            ]
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Add" ]
        ]
