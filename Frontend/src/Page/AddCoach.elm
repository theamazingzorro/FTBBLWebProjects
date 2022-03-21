module Page.AddCoach exposing (Model, Msg, init, update, view)

import Api
import Error exposing (buildErrorMessage)
import Fcss
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Coach exposing (Coach, coachDecoder, defaultCoach, newCoachEncoder)



-- Types --


type alias Model =
    { coach : Coach
    , submitError : Maybe String
    }


type Msg
    = NameChanged String
    | Submit
    | CoachSubmitted (Result Http.Error Coach)



-- Init --


init : ( Model, Cmd Msg )
init =
    ( { coach = defaultCoach, submitError = Nothing }, Cmd.none )



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
            ( model, submitCoach model.coach )

        CoachSubmitted (Ok _) ->
            ( { model | coach = defaultCoach, submitError = Nothing }, Cmd.none )

        CoachSubmitted (Err err) ->
            ( { model | submitError = Just (buildErrorMessage err) }, Cmd.none )


submitCoach : Coach -> Cmd Msg
submitCoach coach =
    Api.postRequest Api.Coaches
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
            div [ Fcss.errorMessage ]
                [ h3 [] [ text "Couldn't save a coach at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


viewForm : Coach -> Html Msg
viewForm coach =
    div []
        [ div [ Fcss.formEntry ]
            [ label
                (Fcss.formLabel "nameInput")
                [ text "Name" ]
            , input
                (Fcss.formInput "nameInput"
                    [ onInput NameChanged
                    , value coach.name
                    ]
                )
                []
            ]
        , button
            [ Fcss.submitButton
            , onClick Submit
            ]
            [ text "Add" ]
        ]
