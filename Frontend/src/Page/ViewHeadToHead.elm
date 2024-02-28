module Page.ViewHeadToHead exposing (Model, Msg, init, update, view)

import Api
import Custom.Html exposing (..)
import Error
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Coach as Coach exposing (Coach, CoachId, coachsDecoder, defaultCoach)
import Model.Division exposing (Division, DivisionId, compareDivisions)
import Model.Game exposing (Game, gamesDecoder)
import Model.Session exposing (Session)
import Model.Team as Team exposing (Team, TeamId, defaultTeam, teamsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



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
    , useTeams : Bool
    }


type Msg
    = GamesReceived (WebData (List Game))
    | TeamListReceived (WebData (List Team))
    | CoachListReceived (WebData (List Coach))
    | Team1Selected String
    | Team2Selected String
    | Coach1Selected String
    | Coach2Selected String
    | TeamsCoachSwitch
    | ViewDivisionClick DivisionId
    | ViewCoachClick CoachId
    | ViewTeamClick TeamId



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
              , useTeams = True
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
              , useTeams = False
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
              , useTeams = True
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
            ( { model | games = sortGames response }, Cmd.none )

        TeamListReceived response ->
            ( { model | teams = response }, Cmd.none )

        CoachListReceived response ->
            ( { model | coaches = response }, Cmd.none )

        Team1Selected teamId ->
            let
                newTeam =
                    searchForIdString teamId Team.idToString model.teams

                ( loading, cmd ) =
                    maybeGetGamesForTeams model.session.token newTeam model.team2Id

                newGames =
                    if loading then
                        RemoteData.Loading

                    else
                        model.games
            in
            ( { model | team1Id = newTeam, games = newGames }, cmd )

        Team2Selected teamId ->
            let
                newTeam =
                    searchForIdString teamId Team.idToString model.teams

                ( loading, cmd ) =
                    maybeGetGamesForTeams model.session.token model.team1Id newTeam

                newGames =
                    if loading then
                        RemoteData.Loading

                    else
                        model.games
            in
            ( { model | team2Id = newTeam, games = newGames }, cmd )

        Coach1Selected coachId ->
            let
                newCoach =
                    searchForIdString coachId Coach.idToString model.coaches

                ( loading, cmd ) =
                    maybeGetGamesForCoaches model.session.token newCoach model.coach2Id

                newGames =
                    if loading then
                        RemoteData.Loading

                    else
                        RemoteData.NotAsked
            in
            ( { model | coach1Id = newCoach, games = newGames }, cmd )

        Coach2Selected coachId ->
            let
                newCoach =
                    searchForIdString coachId Coach.idToString model.coaches

                ( loading, cmd ) =
                    maybeGetGamesForCoaches model.session.token model.coach1Id newCoach

                newGames =
                    if loading then
                        RemoteData.Loading

                    else
                        RemoteData.NotAsked
            in
            ( { model | coach2Id = newCoach, games = newGames }, cmd )

        TeamsCoachSwitch ->
            let
                newUseTeams =
                    not model.useTeams

                ( loading, newGamesCmd ) =
                    if newUseTeams then
                        maybeGetGamesForTeams model.session.token model.team1Id model.team2Id

                    else
                        maybeGetGamesForCoaches model.session.token model.coach1Id model.coach2Id

                newGames =
                    if loading then
                        RemoteData.Loading

                    else
                        RemoteData.NotAsked
            in
            ( { model | useTeams = newUseTeams, games = newGames }, newGamesCmd )

        ViewDivisionClick divId ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision divId )

        ViewCoachClick coachId ->
            ( model, pushUrl model.session.navkey <| Route.ViewCoach coachId )

        ViewTeamClick teamId ->
            ( model, pushUrl model.session.navkey <| Route.ViewTeam teamId )


sortGames : WebData (List Game) -> WebData (List Game)
sortGames gameData =
    case gameData of
        RemoteData.Success games ->
            RemoteData.Success <| List.sortWith (\a b -> compareDivisions b.division a.division) games

        other ->
            other


searchForIdString : String -> (id -> String) -> RemoteData.RemoteData e (List { d | id : id }) -> Maybe id
searchForIdString idString idToString listData =
    case listData of
        RemoteData.Success list ->
            List.filter (\x -> idToString x.id == idString) list
                |> List.head
                |> Maybe.map .id

        _ ->
            Nothing



-- API Requests --


maybeGetGamesForTeams : Maybe String -> Maybe TeamId -> Maybe TeamId -> ( Bool, Cmd Msg )
maybeGetGamesForTeams token t1 t2 =
    case ( t1, t2 ) of
        ( Just team1, Just team2 ) ->
            ( True, getGamesForTeamsRequest token team1 team2 )

        ( _, _ ) ->
            ( False, Cmd.none )


maybeGetGamesForCoaches : Maybe String -> Maybe CoachId -> Maybe CoachId -> ( Bool, Cmd Msg )
maybeGetGamesForCoaches token c1 c2 =
    case ( c1, c2 ) of
        ( Just coach1, Just coach2 ) ->
            ( True, getGamesForCoachesRequest token coach1 coach2 )

        ( _, _ ) ->
            ( False, Cmd.none )


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



-- Helper Functions --


getTeam : List Team -> Maybe TeamId -> Maybe Team
getTeam teams teamId =
    case teamId of
        Just id ->
            List.filter (\team -> team.id == id) teams
                |> List.head

        _ ->
            Nothing


getCoach : List Coach -> Maybe CoachId -> Maybe Coach
getCoach coaches coachId =
    case coachId of
        Just id ->
            List.filter (\coach -> coach.id == id) coaches
                |> List.head

        _ ->
            Nothing


getRecordForTeams : List Game -> TeamId -> TeamId -> ( Int, Int, Int )
getRecordForTeams games team1Id team2Id =
    let
        isWin teamId game =
            (game.homeTeam.id == teamId && Maybe.withDefault 0 game.homeScore > Maybe.withDefault 0 game.awayScore)
                || (game.awayTeam.id == teamId && Maybe.withDefault 0 game.awayScore > Maybe.withDefault 0 game.homeScore)

        draws =
            List.filter (\g -> g.awayScore == g.homeScore) games
                |> List.length

        t1Wins =
            List.filter (isWin team1Id) games
                |> List.length

        t2Wins =
            List.filter (isWin team2Id) games
                |> List.length
    in
    ( t1Wins, draws, t2Wins )


getRecordForCoaches : List Game -> CoachId -> CoachId -> ( Int, Int, Int )
getRecordForCoaches games coach1Id coach2Id =
    let
        isWin coachId game =
            (game.homeTeam.coach.id == coachId && Maybe.withDefault 0 game.homeScore > Maybe.withDefault 0 game.awayScore)
                || (game.awayTeam.coach.id == coachId && Maybe.withDefault 0 game.awayScore > Maybe.withDefault 0 game.homeScore)

        draws =
            List.filter (\g -> g.awayScore == g.homeScore) games
                |> List.length

        c1Wins =
            List.filter (isWin coach1Id) games
                |> List.length

        c2Wins =
            List.filter (isWin coach2Id) games
                |> List.length
    in
    ( c1Wins, draws, c2Wins )



-- View --


view : Model -> Html Msg
view model =
    if model.useTeams then
        row []
            [ narrowRow [ floatRight ] [ viewSwitchTeamCoachButton "View Coaches" ]
            , narrowRow [] [ mainHeader [ textCenter ] [ text "Versus" ] ]
            , viewTeamSelector model.teams model.team1Id model.team2Id
            , viewTeamsIfSelected model.teams model.team1Id model.team2Id
            , viewTeamHeadToHead model.teams model.games model.team1Id model.team2Id
            , viewGames model.games
            ]

    else
        row []
            [ narrowRow [ floatRight ] [ viewSwitchTeamCoachButton "View Teams" ]
            , narrowRow [] [ mainHeader [ textCenter ] [ text "Versus" ] ]
            , viewCoachSelector model.coaches model.coach1Id model.coach2Id
            , viewCoachesIfSelected model.coaches model.coach1Id model.coach2Id
            , viewCoachHeadToHead model.coaches model.games model.coach1Id model.coach2Id
            , viewGames model.games
            ]


viewSwitchTeamCoachButton : String -> Html Msg
viewSwitchTeamCoachButton buttonText =
    optionButton
        [ onClick TeamsCoachSwitch ]
        [ text buttonText ]


viewDivisionButton : Division -> Html Msg
viewDivisionButton division =
    pageLink
        [ onClick <| ViewDivisionClick division.id ]
        [ text <| division.name ++ " Season " ++ String.fromInt division.season ]


viewCoachButton : Coach -> Html Msg
viewCoachButton coach =
    pageLink
        [ onClick <| ViewCoachClick coach.id ]
        [ text coach.name ]


viewTeamButton : Team -> Html Msg
viewTeamButton team =
    pageLink
        [ onClick <| ViewTeamClick team.id ]
        [ text team.name ]


viewLoadError : String -> Html Msg
viewLoadError errorMessage =
    errorText []
        [ emphasisText [] [ text "Couldn't fetch data at this time." ]
        , text <| "Error: " ++ errorMessage
        ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage message =
    case message of
        Just m ->
            errorText [] [ text <| "Error: " ++ m ]

        Nothing ->
            text ""



-- View Drop Downs --


viewTeamSelector : WebData (List Team) -> Maybe TeamId -> Maybe TeamId -> Html Msg
viewTeamSelector teamData team1Id team2Id =
    row []
        [ colThird [] [ viewDropdown team1Id teamData <| teamDropdown Team1Selected ]
        , colThird [] [ emphasisText [ textCenter ] [ text "vs." ] ]
        , colThird [] [ viewDropdown team2Id teamData <| teamDropdown Team2Selected ]
        ]


viewCoachSelector : WebData (List Coach) -> Maybe CoachId -> Maybe CoachId -> Html Msg
viewCoachSelector coachData coach1 coach2 =
    row []
        [ colThird [] [ viewDropdown coach1 coachData <| coachDropdown Coach1Selected ]
        , colThird [] [ emphasisText [ textCenter ] [ text "vs." ] ]
        , colThird [] [ viewDropdown coach2 coachData <| coachDropdown Coach2Selected ]
        ]


viewDropdown : Maybe id -> WebData (List { obj | id : id, name : comparable }) -> (Maybe id -> List { obj | id : id, name : comparable } -> Html Msg) -> Html Msg
viewDropdown idData data dropDownFunction =
    case data of
        RemoteData.NotAsked ->
            dropDownFunction Nothing []

        RemoteData.Loading ->
            emphasisText [] [ text "Loading Options..." ]

        RemoteData.Failure httpError ->
            errorText [] [ text <| "Cannot load Options. " ++ Error.buildErrorMessage httpError ]

        RemoteData.Success d ->
            dropDownFunction idData <| List.sortBy .name d


teamDropdown : (String -> Msg) -> Maybe TeamId -> List Team -> Html Msg
teamDropdown selectionEvent selectedTeam teams =
    inputSection []
        [ dropdownInput [ onInput selectionEvent ]
            (List.map (teamOption selectedTeam) teams)
        , inputLabel [] [ text "" ]
        ]


coachDropdown : (String -> Msg) -> Maybe CoachId -> List Coach -> Html Msg
coachDropdown selectionEvent selectedCoach coaches =
    inputSection []
        [ dropdownInput [ onInput selectionEvent ]
            (List.map (coachOption selectedCoach) coaches)
        , inputLabel [] [ text "" ]
        ]


teamOption : Maybe TeamId -> Team -> ( List (Attribute msg), List (Html msg) )
teamOption currentTeam team =
    ( [ value <| Team.idToString team.id
      , selected (team.id == Maybe.withDefault defaultTeam.id currentTeam)
      ]
    , [ text team.name ]
    )


coachOption : Maybe CoachId -> Coach -> ( List (Attribute msg), List (Html msg) )
coachOption currentCoach coach =
    ( [ value <| Coach.idToString coach.id
      , selected (coach.id == Maybe.withDefault defaultCoach.id currentCoach)
      ]
    , [ text coach.name ]
    )



-- View Team / Coach Specifics --


viewTeamsIfSelected : WebData (List Team) -> Maybe TeamId -> Maybe TeamId -> Html Msg
viewTeamsIfSelected teamData team1Id team2Id =
    case teamData of
        RemoteData.Success teams ->
            viewTeams (getTeam teams team1Id) (getTeam teams team2Id)

        _ ->
            text ""


viewCoachesIfSelected : WebData (List Coach) -> Maybe CoachId -> Maybe CoachId -> Html Msg
viewCoachesIfSelected coachData coach1Id coach2Id =
    case coachData of
        RemoteData.Success coaches ->
            viewCoaches (getCoach coaches coach1Id) (getCoach coaches coach2Id)

        _ ->
            text ""


viewTeams : Maybe Team -> Maybe Team -> Html Msg
viewTeams team1 team2 =
    shadedContainer []
        [ colHalf [] [ viewTeam team1 ]
        , colHalf [] [ viewTeam team2 ]
        ]


viewCoaches : Maybe Coach -> Maybe Coach -> Html Msg
viewCoaches coach1 coach2 =
    shadedContainer []
        [ colHalf [] [ viewCoach coach1 ]
        , colHalf [] [ viewCoach coach2 ]
        ]


viewTeam : Maybe Team -> Html Msg
viewTeam tData =
    case tData of
        Just team ->
            floatingCard []
                [ subHeader [] [ text team.name ]
                , bodyText [] [ text "Coach: ", viewCoachButton team.coach ]
                , bodyText [] [ text <| "Race: " ++ team.race.name ]
                , bodyText [] [ text <| "Current Elo: " ++ String.fromInt team.elo ]
                , bodyText [] [ text "Most Recent Division: ", Maybe.map viewDivisionButton team.division |> Maybe.withDefault (text "N/A") ]
                ]

        Nothing ->
            text ""


viewCoach : Maybe Coach -> Html Msg
viewCoach cData =
    case cData of
        Just coach ->
            floatingCard []
                [ subHeader [] [ text coach.name ]
                , bodyText [] [ text <| "Current Elo: " ++ String.fromInt coach.elo ]
                ]

        Nothing ->
            text ""



-- Matchup Record --


viewTeamHeadToHead : WebData (List Team) -> WebData (List Game) -> Maybe TeamId -> Maybe TeamId -> Html Msg
viewTeamHeadToHead teamData gameData team1 team2 =
    case ( gameData, teamData ) of
        ( RemoteData.Success games, RemoteData.Success teams ) ->
            viewTeamMatchupRecord teams games team1 team2

        ( _, RemoteData.Loading ) ->
            emphasisText [] [ text "Loading Teams..." ]

        ( RemoteData.Loading, _ ) ->
            emphasisText [] [ text "Loading Games..." ]

        ( _, RemoteData.Failure httpError ) ->
            viewLoadError <| Error.buildErrorMessage httpError

        ( RemoteData.Failure httpError, _ ) ->
            viewLoadError <| Error.buildErrorMessage httpError

        ( RemoteData.Success _, RemoteData.NotAsked ) ->
            viewErrorMessage <| Just "Error: Team Data Not Requested"

        ( RemoteData.NotAsked, _ ) ->
            text ""


viewCoachHeadToHead : WebData (List Coach) -> WebData (List Game) -> Maybe CoachId -> Maybe CoachId -> Html Msg
viewCoachHeadToHead coachData gameData coach1 coach2 =
    case ( gameData, coachData ) of
        ( RemoteData.Success games, RemoteData.Success coaches ) ->
            viewCoachMatchupRecord coaches games coach1 coach2

        ( _, RemoteData.Loading ) ->
            emphasisText [] [ text "Loading Coaches..." ]

        ( RemoteData.Loading, _ ) ->
            emphasisText [] [ text "Loading Games..." ]

        ( _, RemoteData.Failure httpError ) ->
            viewLoadError <| Error.buildErrorMessage httpError

        ( RemoteData.Failure httpError, _ ) ->
            viewLoadError <| Error.buildErrorMessage httpError

        ( RemoteData.Success _, RemoteData.NotAsked ) ->
            viewErrorMessage <| Just "Error: Coach Data Not Requested"

        ( RemoteData.NotAsked, _ ) ->
            text ""


viewTeamMatchupRecord : List Team -> List Game -> Maybe TeamId -> Maybe TeamId -> Html Msg
viewTeamMatchupRecord teams games team1 team2 =
    case ( team1, team2 ) of
        ( Just team1Id, Just team2Id ) ->
            div []
                [ mainHeader [ textCenter ] [ text "Record" ]
                , row []
                    [ colThird []
                        [ subHeader [ textRight ]
                            [ text <| (getTeam teams team1 |> Maybe.map .name |> Maybe.withDefault "") ]
                        ]
                    , colThird []
                        [ emphasisText [ textCenter ]
                            [ viewRecord <| getRecordForTeams games team1Id team2Id ]
                        ]
                    , colThird []
                        [ subHeader [ textLeft ]
                            [ text <| (getTeam teams team2 |> Maybe.map .name |> Maybe.withDefault "") ]
                        ]
                    ]
                ]

        _ ->
            text ""


viewCoachMatchupRecord : List Coach -> List Game -> Maybe CoachId -> Maybe CoachId -> Html Msg
viewCoachMatchupRecord coaches games coach1 coach2 =
    case ( coach1, coach2 ) of
        ( Just coach1Id, Just coach2Id ) ->
            div []
                [ mainHeader [ textCenter ] [ text "Record" ]
                , row []
                    [ colThird []
                        [ subHeader [ textRight ]
                            [ text <| (getCoach coaches coach1 |> Maybe.map .name |> Maybe.withDefault "") ]
                        ]
                    , colThird []
                        [ emphasisText [ textCenter ]
                            [ viewRecord <| getRecordForCoaches games coach1Id coach2Id ]
                        ]
                    , colThird []
                        [ subHeader [ textLeft ]
                            [ text <| (getCoach coaches coach2 |> Maybe.map .name |> Maybe.withDefault "") ]
                        ]
                    ]
                ]

        _ ->
            text ""


viewRecord : ( Int, Int, Int ) -> Html Msg
viewRecord ( win, draw, loss ) =
    text <| String.fromInt win ++ " - " ++ String.fromInt draw ++ " - " ++ String.fromInt loss



-- View Games List --


viewGames : WebData (List Game) -> Html Msg
viewGames gameData =
    case gameData of
        RemoteData.Success games ->
            div []
                [ subHeader [ textCenter ] [ text "History" ]
                , shadedContainer []
                    (List.map viewGameDetails games)
                ]

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading Games..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewGameDetails : Game -> Html Msg
viewGameDetails game =
    floatingCard []
        [ narrowRow [ textCenter ] [ emphasisText [] [ viewDivisionButton game.division ] ]
        , narrowRow []
            [ colThird [ textRight ]
                [ smallColorText [] [ viewTeamButton game.homeTeam ]
                , bodyText [] [ viewCoachButton game.homeTeam.coach ]
                ]
            , colThird [ textCenter ]
                [ row [] [ viewScore game ] ]
            , colThird [ textLeft ]
                [ smallColorText [] [ viewTeamButton game.awayTeam ]
                , bodyText [] [ viewCoachButton game.awayTeam.coach ]
                ]
            ]
        ]


viewScore : Game -> Html Msg
viewScore game =
    case ( game.homeScore, game.awayScore ) of
        ( Just homeScore, Just awayScore ) ->
            text <| String.fromInt homeScore ++ " - " ++ String.fromInt awayScore

        _ ->
            text "vs."
