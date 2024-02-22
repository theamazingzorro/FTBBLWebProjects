module Page.ViewHeadToHead exposing (Model, Msg, init, update, view)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Model.Coach as Coach exposing (Coach, CoachId, coachsDecoder)
import Model.Game exposing (Game, gamesDecoder)
import Model.Session exposing (Session)
import Model.Team as Team exposing (Team, TeamId, defaultTeam, teamsDecoder)
import RemoteData exposing (WebData)



-- Types --


type alias Model =
    { session : Session
    , team1Id : Maybe TeamId
    , team2Id : Maybe TeamId
    , coach1Id : Maybe CoachId
    , coach2Id : Maybe CoachId
    , games : WebData (List Game)
    , teams : WebData (List Team)
    , coaches : WebData (List Coach)
    }


type Msg
    = GamesReceived (WebData (List Game))
    | TeamListReceived (WebData (List Team))
    | CoachListReceived (WebData (List Coach))
    | Team1Selected String
    | Team2Selected String
    | Coach1Selected String
    | Coach2Selected String



-- Init --


init : Session -> Maybe ( TeamId, TeamId ) -> Maybe ( CoachId, CoachId ) -> ( Model, Cmd Msg )
init session teams coaches =
    case ( teams, coaches ) of
        ( Just ( team1, team2 ), _ ) ->
            ( { session = session
              , team1Id = Just team1
              , team2Id = Just team2
              , coach1Id = Nothing
              , coach2Id = Nothing
              , games = RemoteData.Loading
              , teams = RemoteData.Loading
              , coaches = RemoteData.Loading
              }
            , Cmd.batch
                [ getGamesForTeamsRequest session.token team1 team2
                , getTeamListRequest session.token
                , getCoachListRequest session.token
                ]
            )

        ( _, Just ( coach1, coach2 ) ) ->
            ( { session = session
              , team1Id = Nothing
              , team2Id = Nothing
              , coach1Id = Just coach1
              , coach2Id = Just coach2
              , games = RemoteData.Loading
              , teams = RemoteData.Loading
              , coaches = RemoteData.Loading
              }
            , Cmd.batch
                [ getGamesForCoachesRequest session.token coach1 coach2
                , getTeamListRequest session.token
                , getCoachListRequest session.token
                ]
            )

        _ ->
            ( { session = session
              , team1Id = Nothing
              , team2Id = Nothing
              , coach1Id = Nothing
              , coach2Id = Nothing
              , games = RemoteData.NotAsked
              , teams = RemoteData.Loading
              , coaches = RemoteData.Loading
              }
            , Cmd.batch
                [ getTeamListRequest session.token
                , getCoachListRequest session.token
                ]
            )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GamesReceived response ->
            ( { model | games = response }, Cmd.none )

        TeamListReceived response ->
            ( { model | teams = response }, Cmd.none )

        CoachListReceived response ->
            ( { model | coaches = response }, Cmd.none )

        Team1Selected teamId ->
            ( { model | team1Id = searchForIdString teamId Team.idToString model.teams }, Cmd.none )

        Team2Selected teamId ->
            ( { model | team2Id = searchForIdString teamId Team.idToString model.teams }, Cmd.none )

        Coach1Selected coachId ->
            ( { model | coach1Id = searchForIdString coachId Coach.idToString model.coaches }, Cmd.none )

        Coach2Selected coachId ->
            ( { model | coach2Id = searchForIdString coachId Coach.idToString model.coaches }, Cmd.none )



-- API Requests --


getGamesForTeamsRequest : Maybe String -> TeamId -> TeamId -> Cmd Msg
getGamesForTeamsRequest token team1 team2 =
    Api.getRequest token (Api.GamesBetweenTeams team1 team2) <|
        Http.expectJson (RemoteData.fromResult >> GamesReceived) gamesDecoder


getGamesForCoachesRequest : Maybe String -> CoachId -> CoachId -> Cmd Msg
getGamesForCoachesRequest token coach1 coach2 =
    Api.getRequest token (Api.GamesBetweenCoaches coach1 coach2) <|
        Http.expectJson (RemoteData.fromResult >> GamesReceived) gamesDecoder


getTeamListRequest : Maybe String -> Cmd Msg
getTeamListRequest token =
    Api.getRequest token Api.Teams <|
        Http.expectJson (RemoteData.fromResult >> TeamListReceived) teamsDecoder


getCoachListRequest : Maybe String -> Cmd Msg
getCoachListRequest token =
    Api.getRequest token Api.Coaches <|
        Http.expectJson (RemoteData.fromResult >> CoachListReceived) coachsDecoder


searchForIdString : String -> (id -> String) -> RemoteData.RemoteData e (List { d | id : id }) -> Maybe id
searchForIdString idString idToString listData =
    case listData of
        RemoteData.Success list ->
            List.filter (\x -> idToString x.id == idString) list
                |> List.head
                |> Maybe.map .id

        _ ->
            Nothing



-- Helper Functions --


getTeam : WebData (List Team) -> Maybe TeamId -> Maybe Team
getTeam teamData teamId =
    case ( teamData, teamId ) of
        ( RemoteData.Success teams, Just id ) ->
            List.filter (\team -> team.id == id) teams
                |> List.head

        _ ->
            Nothing


getCoach : WebData (List Coach) -> Maybe CoachId -> Maybe Coach
getCoach coachData coachId =
    case ( coachData, coachId ) of
        ( RemoteData.Success coaches, Just id ) ->
            List.filter (\coach -> coach.id == id) coaches
                |> List.head

        _ ->
            Nothing



-- View --


view : Model -> Html Msg
view _ =
    text "skeleton"
