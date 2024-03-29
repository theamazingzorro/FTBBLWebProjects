module Page.AddDivision exposing (Model, Msg, init, update, view)

import Api
import Custom.Events exposing (onEnter)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Http
import Model.Division exposing (Division, defaultDivision, divisionDecoder, newDivisionEncoder)
import Model.Session exposing (Session)



-- Types --


type alias Model =
    { session : Session
    , division : Division
    , submitError : Maybe String
    }


type Msg
    = NameChanged String
    | SeasonChanged String
    | Submit
    | DivisionSubmitted (Result Http.Error Division)



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, division = defaultDivision, submitError = Nothing }, Cmd.none )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChanged newName ->
            let
                rename oldDiv =
                    { oldDiv | name = newName }
            in
            ( { model | division = rename model.division }, Cmd.none )

        SeasonChanged newSeasonText ->
            let
                newSeason =
                    String.toInt newSeasonText
                        |> Maybe.withDefault 0

                reseason oldDiv =
                    { oldDiv | season = newSeason }
            in
            ( { model | division = reseason model.division }, Cmd.none )

        Submit ->
            ( model, submitDivision model.session.token model.division )

        DivisionSubmitted (Ok _) ->
            ( { model | division = defaultDivision, submitError = Nothing }, Cmd.none )

        DivisionSubmitted (Err err) ->
            ( { model | submitError = Just (buildErrorMessage err) }, Cmd.none )



-- API Requests --


submitDivision : Maybe String -> Division -> Cmd Msg
submitDivision token div =
    Api.postRequest token
        Api.Divisions
        (Http.jsonBody (newDivisionEncoder div))
    <|
        Http.expectJson DivisionSubmitted divisionDecoder



-- View --


view : Model -> Html Msg
view model =
    row []
        [ mainHeader [] [ text "Add Division" ]
        , viewSubmitError model.submitError
        , viewForm model
        ]


viewSubmitError : Maybe String -> Html msg
viewSubmitError maybeError =
    case maybeError of
        Just error ->
            errorText []
                [ emphasisText [] [ text "Couldn't save a division at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""


viewForm : Model -> Html Msg
viewForm model =
    inputForm []
        [ viewNameField model.division
        , viewSeasonField model.division
        , submitButton Submit [ text "Add" ]
        ]


viewNameField : Division -> Html Msg
viewNameField division =
    textInput
        [ onInput NameChanged
        , value division.name
        ]
        [ text "Name" ]


viewSeasonField : Division -> Html Msg
viewSeasonField division =
    textInput
        [ onInput SeasonChanged
        , onEnter Submit
        , value <| String.fromInt division.season
        ]
        [ text "Season" ]
