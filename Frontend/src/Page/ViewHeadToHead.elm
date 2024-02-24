module Page.ViewHeadToHead exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
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
                team1Id =
                    searchForIdString teamId Team.idToString model.teams

                ( loading, cmd ) =
                    maybeGetGamesForTeams model.session.token team1Id model.team2Id

                newGames =
                    if loading then
                        RemoteData.Loading

                    else
                        model.games
            in
            ( { model | team1Id = team1Id, games = newGames }, cmd )

        Team2Selected teamId ->
            let
                team2Id =
                    searchForIdString teamId Team.idToString model.teams

                ( loading, cmd ) =
                    maybeGetGamesForTeams model.session.token model.team1Id team2Id

                newGames =
                    if loading then
                        RemoteData.Loading

                    else
                        model.games
            in
            ( { model | team2Id = team2Id, games = newGames }, cmd )

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
                        model.games
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
                        model.games
            in
            ( { model | coach2Id = newCoach, games = newGames }, cmd )

        TeamsCoachSwitch ->
            ( { model | useTeams = not model.useTeams }, Cmd.none )

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


getRecordForTeams : List Game -> TeamId -> TeamId -> (Int, Int, Int)
getRecordForTeams games team1Id team2Id =
    let
        isWin : TeamId -> Game -> Bool
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
    (t1Wins, draws, t2Wins)


getRecordForCoaches : List Game -> CoachId -> CoachId -> (Int, Int, Int)
getRecordForCoaches games coach1Id coach2Id =
    let
        isWin : CoachId -> Game -> Bool
        isWin coachId game =
            (game.homeTeam.coach.id == coachId && Maybe.withDefault 0 game.homeScore > Maybe.withDefault 0 game.awayScore)
                || (game.awayTeam.coach.id == coachId && Maybe.withDefault 0 game.awayScore > Maybe.withDefault 0 game.homeScore)

        draws =
            List.filter (\g -> g.awayScore == g.homeScore) games
                |> List.length

        t1Wins =
            List.filter (isWin coach1Id) games
                |> List.length

        t2Wins =
            List.filter (isWin coach2Id) games
                |> List.length
    in
    (t1Wins, draws, t2Wins)



-- View --


view : Model -> Html Msg
view model =
    if model.useTeams then
        div []
            [ br [] []
            , viewTeamSelector model.teams model.team1Id model.team2Id
            , br [] []
            , viewGames model model.games
            ]

    else
        div []
            [ br [] []
            , viewCoachSelector model.coaches model.coach1Id model.coach2Id
            , br [] []
            , viewGames model model.games
            ]


viewLoadError : String -> Html Msg
viewLoadError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div [ Custom.Attributes.errorMessage ]
        [ h3 [] [ text errorHeading ]
        , text <| "Error: " ++ errorMessage
        ]



-- View Games List --


viewGames : Model -> WebData (List Game) -> Html Msg
viewGames model gameData =
    case gameData of
        RemoteData.Success games ->
            div []
                [ h4 [ Custom.Attributes.textCentered ]
                    [ text "History" ]
                , viewMatchupRecord model games
                , div []
                    (List.map viewGameDetails games)
                ]

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Games..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewMatchupRecord : Model -> List Game -> Html Msg
viewMatchupRecord model games =
    case ( model.useTeams, model.team1Id, model.team2Id ) of
        ( True, Just team1, Just team2 ) ->
            h6 [ Custom.Attributes.textCentered ]
                [ viewRecord <| getRecordForTeams games team1 team2]

        ( False, _, _ ) ->
            case ( model.coach1Id, model.coach2Id ) of
                ( Just coach1, Just coach2 ) ->
                    h6 [ Custom.Attributes.textCentered ]
                        [ viewRecord <| getRecordForCoaches games coach1 coach2]

                _ ->
                    text ""

        _ ->
            text ""


viewRecord : (Int, Int, Int) -> Html Msg
viewRecord (win, draw, loss) =
    text <| String.fromInt win ++ " - " ++ String.fromInt draw ++ " - " ++ String.fromInt loss

viewGameDetails : Game -> Html Msg
viewGameDetails game =
    div Custom.Attributes.carouselItemEntry
        [ div [ class "row" ]
            [ h6 [] [ viewDivisionButton game.division ] ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ h6 [] [ viewTeamButton game.homeTeam ]
                , p [] [ viewCoachButton game.homeTeam.coach ]
                ]
            , div [ class "col col-auto", Custom.Attributes.centered ]
                [ viewScore game ]
            , div [ class "col" ]
                [ h6 [] [ viewTeamButton game.awayTeam ]
                , p [] [ viewCoachButton game.awayTeam.coach ]
                ]
            ]
        ]


viewDivisionButton : Division -> Html Msg
viewDivisionButton division =
    span
        (Custom.Attributes.textButton <| ViewDivisionClick division.id)
        [ text <| division.name ++ " Season " ++ String.fromInt division.season ]


viewCoachButton : Coach -> Html Msg
viewCoachButton coach =
    span
        (Custom.Attributes.textButton <| ViewCoachClick coach.id)
        [ text coach.name ]


viewTeamButton : Team -> Html Msg
viewTeamButton team =
    span
        (Custom.Attributes.textButton <| ViewTeamClick team.id)
        [ text team.name ]


viewScore : Game -> Html Msg
viewScore game =
    case game.homeScore of
        Just homeScore ->
            case game.awayScore of
                Just awayScore ->
                    text <| String.fromInt homeScore ++ " - " ++ String.fromInt awayScore

                Nothing ->
                    text "vs"

        Nothing ->
            text "vs"



-- View Team / Coach Specifics --


viewTeam : Maybe Team -> Html Msg
viewTeam teamData =
    case teamData of
        Just team ->
            div [ Custom.Attributes.col ]
                [ br [] []
                , p []
                    [ text "Coach: "
                    , viewCoachButton team.coach
                    ]
                , p [] [ text <| "Race: " ++ team.race.name ]
                , p [] [ text <| "Current Elo: " ++ String.fromInt team.elo ]
                , p [] [ text "Most Recent Division: ", Maybe.map viewDivisionButton team.division |> Maybe.withDefault (text "N/A") ]
                ]

        Nothing ->
            div [ Custom.Attributes.col ]
                [ text "-" ]


viewCoach : Maybe Coach -> Html Msg
viewCoach coachData =
    case coachData of
        Just coach ->
            div [ Custom.Attributes.col ]
                [ br [] []
                , p [] [ text <| "Current Elo: " ++ String.fromInt coach.elo ]
                ]

        Nothing ->
            div [ Custom.Attributes.col ]
                [ text "-" ]



-- View Drop Downs --


viewTeamSelector : WebData (List Team) -> Maybe TeamId -> Maybe TeamId -> Html Msg
viewTeamSelector teamData team1Id team2Id =
    div Custom.Attributes.row
        [ viewDropdown team1Id teamData team1Dropdown
        , text "vs."
        , viewDropdown team2Id teamData team2Dropdown
        ]


viewCoachSelector : WebData (List Coach) -> Maybe CoachId -> Maybe CoachId -> Html Msg
viewCoachSelector coachData coach1 coach2 =
    div Custom.Attributes.row
        [ viewDropdown coach1 coachData coach1Dropdown
        , text "vs."
        , viewDropdown coach2 coachData coach2Dropdown
        ]


viewDropdown : Maybe id -> WebData (List { obj | id : id, name : comparable }) -> (Maybe id -> List { obj | id : id, name : comparable } -> Html Msg) -> Html Msg
viewDropdown idData data dropDownFunction =
    case data of
        RemoteData.NotAsked ->
            dropDownFunction Nothing []

        RemoteData.Loading ->
            h4 [] [ text "Loading Options..." ]

        RemoteData.Failure httpError ->
            h4 [ Custom.Attributes.errorMessage ]
                [ text <| "Cannot load Options. " ++ Error.buildErrorMessage httpError ]

        RemoteData.Success d ->
            dropDownFunction idData <| List.sortBy .name d


team1Dropdown : Maybe TeamId -> List Team -> Html Msg
team1Dropdown selectedTeam teams =
    div [ Custom.Attributes.col ]
        [ select
            (Custom.Attributes.formDropdown "team1Dropdown"
                [ onInput Team1Selected ]
            )
            (defaultOption :: List.map (teamOption selectedTeam) teams)
        , viewTeam <| getTeam teams selectedTeam
        ]


team2Dropdown : Maybe TeamId -> List Team -> Html Msg
team2Dropdown selectedTeam teams =
    div [ Custom.Attributes.col ]
        [ select
            (Custom.Attributes.formDropdown "team2Dropdown"
                [ onInput Team2Selected ]
            )
            (defaultOption :: List.map (teamOption selectedTeam) teams)
        , viewTeam <| getTeam teams selectedTeam
        ]


coach1Dropdown : Maybe CoachId -> List Coach -> Html Msg
coach1Dropdown selectedCoach coaches =
    div [ Custom.Attributes.col ]
        [ select
            (Custom.Attributes.formDropdown "coach1Dropdown"
                [ onInput Coach1Selected ]
            )
            (defaultOption :: List.map (coachOption selectedCoach) coaches)
        , viewCoach <| getCoach coaches selectedCoach
        ]


coach2Dropdown : Maybe CoachId -> List Coach -> Html Msg
coach2Dropdown selectedCoach coaches =
    div [ Custom.Attributes.col ]
        [ select
            (Custom.Attributes.formDropdown "coach2Dropdown"
                [ onInput Coach2Selected ]
            )
            (defaultOption :: List.map (coachOption selectedCoach) coaches)
        , viewCoach <| getCoach coaches selectedCoach
        ]


teamOption : Maybe TeamId -> Team -> Html msg
teamOption currentTeam team =
    option
        [ value <| Team.idToString team.id
        , selected (team.id == Maybe.withDefault defaultTeam.id currentTeam)
        ]
        [ text team.name ]


coachOption : Maybe CoachId -> Coach -> Html msg
coachOption currentCoach coach =
    option
        [ value <| Coach.idToString coach.id
        , selected (coach.id == Maybe.withDefault defaultCoach.id currentCoach)
        ]
        [ text coach.name ]


defaultOption : Html Msg
defaultOption =
    option [ value "0" ] [ text "-" ]