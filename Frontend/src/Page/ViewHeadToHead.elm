module Page.ViewHeadToHead exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Model.Coach as Coach exposing (Coach, CoachId, coachsDecoder)
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
    | ViewDivisionButtonClick DivisionId



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
            ( { model | coach1Id = searchForIdString coachId Coach.idToString model.coaches }, Cmd.none )

        Coach2Selected coachId ->
            ( { model | coach2Id = searchForIdString coachId Coach.idToString model.coaches }, Cmd.none )

        TeamsCoachSwitch ->
            ( { model | useTeams = not model.useTeams }, Cmd.none )

        ViewDivisionButtonClick divId ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision divId )


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
view model =
    if model.useTeams then
        div []
            [ br [] []
            , viewTeamSelector model.teams model.team1Id model.team2Id
            , br [] []
            , viewGames model.games
            ]

    else
        text "coach"


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


viewGames : WebData (List Game) -> Html Msg
viewGames gameData =
    case gameData of
        RemoteData.Success games ->
            div []
                [ h4 [ Custom.Attributes.textCentered ]
                    [ text "History" ]
                , div []
                    (List.map viewGameDetails games)
                ]

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Games..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewGameDetails : Game -> Html Msg
viewGameDetails game =
    div Custom.Attributes.carouselItemEntry
        [ div [ class "row" ]
            [ h6 [] [ viewDivision game.division ] ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ h6 [] [ text game.homeTeam.name ]
                , p [] [ text game.homeTeam.coach.name ]
                ]
            , div [ class "col col-auto", Custom.Attributes.centered ]
                [ viewScore game
                ]
            , div [ class "col" ]
                [ h6 [] [ text game.awayTeam.name ]
                , p [] [ text game.awayTeam.coach.name ]
                ]
            ]
        ]


viewDivision : Division -> Html Msg
viewDivision division =
    span
        (Custom.Attributes.textButton <| ViewDivisionButtonClick division.id)
        [ text <| division.name ++ " Season " ++ String.fromInt division.season ]


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



-- View Drop Downs --


viewTeamSelector : WebData (List Team) -> Maybe TeamId -> Maybe TeamId -> Html Msg
viewTeamSelector teamData team1Id team2Id =
    div Custom.Attributes.row
        [ viewDropdown team1Id teamData team1Dropdown
        , text "vs."
        , viewDropdown team2Id teamData team2Dropdown
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
        ]


team2Dropdown : Maybe TeamId -> List Team -> Html Msg
team2Dropdown selectedTeam teams =
    div [ Custom.Attributes.col ]
        [ select
            (Custom.Attributes.formDropdown "team2Dropdown"
                [ onInput Team2Selected ]
            )
            (defaultOption :: List.map (teamOption selectedTeam) teams)
        ]


teamOption : Maybe TeamId -> Team -> Html msg
teamOption currentTeam team =
    option
        [ value <| Team.idToString team.id
        , selected (team.id == Maybe.withDefault defaultTeam.id currentTeam)
        ]
        [ text team.name ]


defaultOption : Html Msg
defaultOption =
    option [ value "0" ] [ text "-" ]
