module Page.AddDivision exposing (Model, Msg, init, update, view)

import Api
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Model.Division exposing (Division, defaultDivision, divisionDecoder, newDivisionEncoder)
import Custom.Attributes
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)
import Custom.Events exposing (onEnter)



-- Types --


type alias Model =
    { division : Division
    , submitError : Maybe String
    }


type Msg
    = NameChanged String
    | SeasonChanged String
    | Submit
    | DivisionSubmitted (Result Http.Error Division)



-- Init --


init : ( Model, Cmd Msg )
init =
    ( { division = defaultDivision, submitError = Nothing }, Cmd.none )



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
            ( model, submitDivision model.division )

        DivisionSubmitted (Ok _) ->
            ( { model | division = defaultDivision, submitError = Nothing }, Cmd.none )

        DivisionSubmitted (Err err) ->
            ( { model | submitError = Just (buildErrorMessage err) }, Cmd.none )



-- API Requests --


submitDivision : Division -> Cmd Msg
submitDivision div =
    Api.postRequest Api.Divisions
        (Http.jsonBody (newDivisionEncoder div))
    <|
        Http.expectJson DivisionSubmitted divisionDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Add Division" ]
        , br [] []
        , viewSubmitError model.submitError
        , viewForm model
        ]


viewSubmitError : Maybe String -> Html msg
viewSubmitError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a division at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ viewNameField model.division
        , viewSeasonField model.division
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Add" ]
        ]

viewNameField : Division -> Html Msg
viewNameField division =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "nameInput")
            [ text "Name" ]
        , input
            (Custom.Attributes.formInput "nameInput"
                [ onInput NameChanged
                , value division.name
                ]
            )
            []
        ]


viewSeasonField : Division -> Html Msg
viewSeasonField division =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "seasonInput")
            [ text "Season" ]
        , input
            (Custom.Attributes.formInput "seasonInput"
                [ onInput SeasonChanged
                , onEnter Submit
                , value <| String.fromInt division.season
                ]
            )
            []
        ]