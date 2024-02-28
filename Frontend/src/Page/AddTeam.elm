module Page.AddTeam exposing (Model, Msg, init, update, view)

import Api
import Custom.Events exposing (onEnter)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Coach as Coach exposing (Coach, coachsDecoder)
import Model.Race as Race exposing (Race, racesDecoder)
import Model.Session exposing (Session)
import Model.Team exposing (Team, defaultTeam, newTeamEncoder, teamDecoder)
import RemoteData exposing (WebData)



-- Types --


type alias Model =
    { session : Session
    , team : Team
    , raceOptions : WebData (List Race)
    , coachOptions : WebData (List Coach)
    , submitError : Maybe String
    }


type Msg
    = RacesRecieved (WebData (List Race))
    | CoachesReceived (WebData (List Coach))
    | NameChanged String
    | RaceSelected String
    | CoachSelected String
    | Submit
    | TeamSubmitted (Result Http.Error Team)



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , team = defaultTeam
      , raceOptions = RemoteData.Loading
      , coachOptions = RemoteData.Loading
      , submitError = Nothing
      }
    , Cmd.batch [ getRacesRequest session.token, getCoachesRequest session.token ]
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RacesRecieved response ->
            ( { model | raceOptions = response }, Cmd.none )

        CoachesReceived response ->
            ( { model | coachOptions = response }, Cmd.none )

        NameChanged newName ->
            let
                rename oldTeam =
                    { oldTeam | name = newName }
            in
            ( { model | team = rename model.team }, Cmd.none )

        RaceSelected idString ->
            let
                newRace =
                    case model.raceOptions of
                        RemoteData.Success races ->
                            searchByIdString idString Race.idToString model.team.race races

                        _ ->
                            model.team.race

                changeRace oldTeam =
                    { oldTeam | race = newRace }
            in
            ( { model | team = changeRace model.team }, Cmd.none )

        CoachSelected idString ->
            let
                newCoach =
                    case model.coachOptions of
                        RemoteData.Success coaches ->
                            searchByIdString idString Coach.idToString model.team.coach coaches

                        _ ->
                            model.team.coach

                changeCoach oldTeam =
                    { oldTeam | coach = newCoach }
            in
            ( { model | team = changeCoach model.team }, Cmd.none )

        Submit ->
            ( model, submitTeam model.session.token model.team )

        TeamSubmitted (Ok _) ->
            ( { model | team = defaultTeam, submitError = Nothing }, Cmd.none )

        TeamSubmitted (Err err) ->
            ( { model | submitError = Just (buildErrorMessage err) }, Cmd.none )


searchByIdString : String -> (id -> String) -> { c | id : id } -> List { c | id : id } -> { c | id : id }
searchByIdString idString stringFromId defaultVal list =
    List.filter (\item -> stringFromId item.id == idString) list
        |> List.head
        |> Maybe.withDefault defaultVal



-- API Requests --


submitTeam : Maybe String -> Team -> Cmd Msg
submitTeam token team =
    Api.postRequest token
        Api.Teams
        (Http.jsonBody (newTeamEncoder team))
    <|
        Http.expectJson TeamSubmitted teamDecoder


getRacesRequest : Maybe String -> Cmd Msg
getRacesRequest token =
    Api.getRequest token Api.Races <|
        Http.expectJson (RemoteData.fromResult >> RacesRecieved) racesDecoder


getCoachesRequest : Maybe String -> Cmd Msg
getCoachesRequest token =
    Api.getRequest token Api.Coaches <|
        Http.expectJson (RemoteData.fromResult >> CoachesReceived) coachsDecoder



-- View --


view : Model -> Html Msg
view model =
    row []
        [ mainHeader [] [ text "Add Team" ]
        , viewSubmitError model.submitError
        , viewForm model
        ]


viewSubmitError : Maybe String -> Html msg
viewSubmitError maybeError =
    case maybeError of
        Just error ->
            errorText []
                [ emphasisText [] [ text "Couldn't save a team at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""


viewForm : Model -> Html Msg
viewForm model =
    inputForm []
        [ viewNameField model.team
        , viewRaceField model.team model.raceOptions
        , viewCoachField model.team model.coachOptions
        , addButton [ onClick Submit ] [ text "Add" ]
        ]


viewNameField : Team -> Html Msg
viewNameField team =
    textInput
        [ onInput NameChanged
        , onEnter Submit
        , value team.name
        ]
        [ text "Name" ]


viewRaceField : Team -> WebData (List Race) -> Html Msg
viewRaceField team data =
    case data of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading Options..." ]

        RemoteData.Failure httpError ->
            errorText [] [ text <| "Cannot load Options. " ++ Error.buildErrorMessage httpError ]

        RemoteData.Success races ->
            raceDropdown team <| List.sortBy .name races


viewCoachField : Team -> WebData (List Coach) -> Html Msg
viewCoachField team data =
    case data of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading Options..." ]

        RemoteData.Failure httpError ->
            errorText [] [ text <| "Cannot load Options. " ++ Error.buildErrorMessage httpError ]

        RemoteData.Success coaches ->
            coachDropdown team <| List.sortBy .name coaches


raceDropdown : Team -> List Race -> Html Msg
raceDropdown team races =
    inputSection []
        [ dropdownInput [ onInput RaceSelected ]
            (List.map (raceOption team) races)
        , inputLabel [] [ text "Race" ]
        ]


coachDropdown : Team -> List Coach -> Html Msg
coachDropdown team coaches =
    inputSection []
        [ dropdownInput [ onInput CoachSelected ]
            (List.map (coachOption team) coaches)
        , inputLabel [] [ text "Coach" ]
        ]


raceOption : Team -> Race -> ( List (Attribute msg), List (Html msg) )
raceOption team race =
    ( [ value <| Race.idToString race.id
      , selected (race.id == team.race.id)
      ]
    , [ text race.name ]
    )


coachOption : Team -> Coach -> ( List (Attribute msg), List (Html msg) )
coachOption team coach =
    ( [ value <| Coach.idToString coach.id
      , selected (coach.id == team.coach.id)
      ]
    , [ text coach.name ]
    )
