module Page.AddTeam exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    div []
        [ h3 [] [ text "Add Team" ]
        , br [] []
        , viewSubmitError model.submitError
        , viewForm model
        ]


viewSubmitError : Maybe String -> Html msg
viewSubmitError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a team at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ viewNameField model.team
        , viewRaceField model.team model.raceOptions
        , viewCoachField model.team model.coachOptions
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Add" ]
        ]


viewNameField : Team -> Html Msg
viewNameField team =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "nameInput")
            [ text "Name" ]
        , input
            (Custom.Attributes.formInput "nameInput"
                [ onInput NameChanged
                , onEnter Submit
                , value team.name
                ]
            )
            []
        ]


viewRaceField : Team -> WebData (List Race) -> Html Msg
viewRaceField team data =
    case data of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h4 [] [ text "Loading Options..." ]

        RemoteData.Failure httpError ->
            h4 [ Custom.Attributes.errorMessage ]
                [ text <| "Cannot load Options. " ++ Error.buildErrorMessage httpError ]

        RemoteData.Success races ->
            raceDropdown team races


raceDropdown : Team -> List Race -> Html Msg
raceDropdown team races =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "raceDropdown")
            [ text "Race" ]
        , select
            (Custom.Attributes.formDropdown "raceDropdown"
                [ onInput RaceSelected ]
            )
            (defaultOption :: List.map (raceOption team) races)
        ]


viewCoachField : Team -> WebData (List Coach) -> Html Msg
viewCoachField team data =
    case data of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h4 [] [ text "Loading Options..." ]

        RemoteData.Failure httpError ->
            h4 [ Custom.Attributes.errorMessage ]
                [ text <| "Cannot load Options. " ++ Error.buildErrorMessage httpError ]

        RemoteData.Success coaches ->
            coachDropdown team coaches


coachDropdown : Team -> List Coach -> Html Msg
coachDropdown team coaches =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "coachDropdown")
            [ text "Coach" ]
        , select
            (Custom.Attributes.formDropdown "coachDropdown"
                [ onInput CoachSelected ]
            )
            (defaultOption :: List.map (coachOption team) coaches)
        ]


defaultOption : Html Msg
defaultOption =
    option [ value "0" ] [ text "-" ]


raceOption : Team -> Race -> Html msg
raceOption team race =
    option
        [ value <| Race.idToString race.id
        , selected (race.id == team.race.id)
        ]
        [ text race.name ]


coachOption : Team -> Coach -> Html msg
coachOption team coach =
    option
        [ value <| Coach.idToString coach.id
        , selected (coach.id == team.coach.id)
        ]
        [ text coach.name ]
