module Page.ViewDivision exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, div, h2, text)
import Html.Events exposing (onClick)
import Http
import Model.Accolade exposing (Accolade, viewAccolade)
import Model.Coach exposing (CoachId)
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Division exposing (Division, DivisionId, divisionDecoder)
import Model.Game as Game exposing (Game, GameId, gamesDecoder)
import Model.Session exposing (Session)
import Model.Standing exposing (Standing, compareStandings, getGamesPlayed, getPoints, getTDD, standingsDecoder)
import Model.Team exposing (Team, TeamId)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)
import String exposing (toLower)
import Url exposing (Protocol(..))



-- Types --


type alias Model =
    { standings : WebData (List Standing)
    , sortingMethod : TeamSortingMethod
    , division : WebData Division
    , games : WebData (List Game)
    , displayedWeek : Int
    , divisionId : DivisionId
    , session : Session
    , deleteError : Maybe String
    }


type Msg
    = RefreshButtonClick
    | StandingsReceived (WebData (List Standing))
    | GamesReceived (WebData (List Game))
    | DivisionReceived (WebData Division)
    | AddTeamButtonClick
    | DeleteTeamButtonClick TeamId
    | EditTeamButtonClick TeamId
    | ViewTeamClick TeamId
    | ViewCoachClick CoachId
    | TeamDeleted (Result Http.Error DeleteResponse)
    | GameDeleted (Result Http.Error DeleteResponse)
    | TeamNameSortClick
    | TeamRaceSortClick
    | TeamCoachSortClick
    | TeamEloSortClick
    | DefaultSortClick
    | ChangeWeek Int
    | DeleteGameButtonClick GameId
    | EditGameButtonClick GameId
    | AddGameButtonClick Int
    | AddWeekButtonClick Int
    | FirstPageClick
    | PrevPageClick
    | NextPageClick
    | LastPageClick


type TeamSortingMethod
    = Default
    | Name
    | NameDesc
    | Race
    | RaceDesc
    | Coach
    | CoachDesc
    | Elo
    | EloDesc



-- Init --


init : Session -> DivisionId -> Maybe Int -> ( Model, Cmd Msg )
init session id startWeek =
    ( { standings = RemoteData.Loading
      , sortingMethod = Default
      , division = RemoteData.Loading
      , games = RemoteData.Loading
      , displayedWeek = Maybe.withDefault -1 startWeek
      , session = session
      , deleteError = Nothing
      , divisionId = id
      }
    , Cmd.batch
        [ getStandingsRequest session.token id
        , getDivisionRequest session.token id
        , getGamesRequest session.token id
        ]
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RefreshButtonClick ->
            ( { model
                | standings = RemoteData.Loading
                , division = RemoteData.Loading
              }
            , Cmd.batch
                [ getStandingsRequest model.session.token model.divisionId
                , getDivisionRequest model.session.token model.divisionId
                , getGamesRequest model.session.token model.divisionId
                ]
            )

        StandingsReceived response ->
            ( { model | standings = response }, Cmd.none )

        GamesReceived response ->
            ( { model | games = response, displayedWeek = getStartingWeek model.displayedWeek response }, Cmd.none )

        DivisionReceived response ->
            ( { model | division = response }, Cmd.none )

        AddTeamButtonClick ->
            ( model, pushUrl model.session.navkey <| Route.AddTeamToDivision model.divisionId )

        EditTeamButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.EditTeam id )

        ViewTeamClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewTeam id )

        ViewCoachClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewCoach id )

        DeleteTeamButtonClick id ->
            ( model, deleteTeamRequest model.session.token id )

        TeamDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getStandingsRequest model.session.token model.divisionId )

        TeamDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )

        GameDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getGamesRequest model.session.token model.divisionId )

        GameDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )

        TeamNameSortClick ->
            ( { model | sortingMethod = newSort Name NameDesc model.sortingMethod }, Cmd.none )

        TeamRaceSortClick ->
            ( { model | sortingMethod = newSort Race RaceDesc model.sortingMethod }, Cmd.none )

        TeamCoachSortClick ->
            ( { model | sortingMethod = newSort Coach CoachDesc model.sortingMethod }, Cmd.none )

        DefaultSortClick ->
            ( { model | sortingMethod = Default }, Cmd.none )

        TeamEloSortClick ->
            ( { model | sortingMethod = newSort EloDesc Elo model.sortingMethod }, Cmd.none )

        ChangeWeek newWeek ->
            ( { model | displayedWeek = newWeek }, Cmd.none )

        DeleteGameButtonClick id ->
            ( model, deleteGameRequest model.session.token id )

        EditGameButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.EditGame id )

        AddGameButtonClick week ->
            ( model, pushUrl model.session.navkey <| Route.AddGameWithDefaults model.divisionId week )

        AddWeekButtonClick week ->
            ( model, pushUrl model.session.navkey <| Route.AddGameWeek model.divisionId week )

        FirstPageClick ->
            ( { model | displayedWeek = 1 }, Cmd.none )

        PrevPageClick ->
            let
                newWeek =
                    if model.displayedWeek <= 1 then
                        1

                    else
                        model.displayedWeek - 1
            in
            ( { model | displayedWeek = newWeek }, Cmd.none )

        NextPageClick ->
            let
                max =
                    case model.games of
                        RemoteData.Success games ->
                            maxWeek games

                        _ ->
                            model.displayedWeek

                newWeek =
                    if model.displayedWeek >= max then
                        max

                    else
                        model.displayedWeek + 1
            in
            ( { model | displayedWeek = newWeek }, Cmd.none )

        LastPageClick ->
            let
                newWeek =
                    case model.games of
                        RemoteData.Success games ->
                            maxWeek games

                        _ ->
                            model.displayedWeek
            in
            ( { model | displayedWeek = newWeek }, Cmd.none )


newSort : sortMethod -> sortMethod -> sortMethod -> sortMethod
newSort default alt oldSort =
    if oldSort == default then
        alt

    else
        default


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. Team not found."


getStartingWeek : Int -> WebData (List Game) -> Int
getStartingWeek default gameData =
    if default == -1 then
        case gameData of
            RemoteData.Success games ->
                List.filter (\game -> game.homeOdds /= Nothing) games
                    |> List.map (\game -> game.week)
                    |> List.minimum
                    |> Maybe.withDefault (maxWeek games)

            _ ->
                default

    else
        default



-- API Requests --


getStandingsRequest : Maybe String -> DivisionId -> Cmd Msg
getStandingsRequest token divId =
    Api.getRequest token (Api.Standings divId) <|
        Http.expectJson (RemoteData.fromResult >> StandingsReceived) standingsDecoder


getGamesRequest : Maybe String -> DivisionId -> Cmd Msg
getGamesRequest token divId =
    Api.getRequest token (Api.GamesInDiv divId) <|
        Http.expectJson (RemoteData.fromResult >> GamesReceived) gamesDecoder


getDivisionRequest : Maybe String -> DivisionId -> Cmd Msg
getDivisionRequest token divId =
    Api.getRequest token (Api.Division divId) <|
        Http.expectJson (RemoteData.fromResult >> DivisionReceived) divisionDecoder


deleteTeamRequest : Maybe String -> TeamId -> Cmd Msg
deleteTeamRequest token id =
    Api.deleteRequest token (Api.Team id) <|
        Http.expectJson TeamDeleted deleteResponseDecoder


deleteGameRequest : Maybe String -> GameId -> Cmd Msg
deleteGameRequest token id =
    Api.deleteRequest token (Api.Game id) <|
        Http.expectJson GameDeleted deleteResponseDecoder



-- Helper Functions --


sortedStandings : TeamSortingMethod -> List Standing -> List Standing
sortedStandings sortingMethod standings =
    case sortingMethod of
        Default ->
            List.sortWith compareStandings standings

        Name ->
            List.sortWith (\a b -> compareStrIgnoreCase a.team.name b.team.name) standings

        NameDesc ->
            List.sortWith (\a b -> compareStrIgnoreCase b.team.name a.team.name) standings

        Coach ->
            List.sortWith (\a b -> compareStrIgnoreCase a.team.coach.name b.team.coach.name) standings

        CoachDesc ->
            List.sortWith (\a b -> compareStrIgnoreCase b.team.coach.name a.team.coach.name) standings

        Race ->
            List.sortWith (\a b -> compareStrIgnoreCase a.team.race.name b.team.race.name) standings

        RaceDesc ->
            List.sortWith (\a b -> compareStrIgnoreCase b.team.race.name a.team.race.name) standings

        Elo ->
            List.sortWith (\a b -> compare a.team.elo b.team.elo) standings

        EloDesc ->
            List.sortWith (\a b -> compare b.team.elo a.team.elo) standings


compareStrIgnoreCase : String -> String -> Order
compareStrIgnoreCase a b =
    compare (toLower a) (toLower b)


gamesInWeek : Int -> List Game -> List Game
gamesInWeek week games =
    List.filter (\game -> game.week == week) games


maxWeek : List Game -> Int
maxWeek games =
    List.map (\game -> game.week) games
        |> List.maximum
        |> Maybe.withDefault 0



-- View --


view : Model -> Html Msg
view model =
    row []
        [ viewRefreshButton
        , viewErrorMessage model.deleteError
        , viewStandingsOrError model
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    optionButton
        [ onClick RefreshButtonClick, floatRight ]
        [ text "Refresh Page" ]



{- View ErrorHandling -}


viewStandingsOrError : Model -> Html Msg
viewStandingsOrError model =
    case ( model.standings, model.division ) of
        ( RemoteData.NotAsked, _ ) ->
            text ""

        ( RemoteData.Loading, _ ) ->
            emphasisText [] [ text "Loading..." ]

        ( RemoteData.Failure httpError, _ ) ->
            viewLoadError <| Error.buildErrorMessage httpError

        ( RemoteData.Success teams, RemoteData.Success division ) ->
            div []
                [ viewDivisionHeader division model.session
                , viewStandings model division.closed teams
                , viewGamesOrError model division
                ]

        ( _, RemoteData.NotAsked ) ->
            text ""

        ( _, RemoteData.Loading ) ->
            emphasisText [] [ text "Loading..." ]

        ( _, RemoteData.Failure httpError ) ->
            viewLoadError <| Error.buildErrorMessage httpError


viewGamesOrError : Model -> Division -> Html Msg
viewGamesOrError model division =
    case model.games of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success games ->
            row []
                [ viewGamesHeader division games model.session
                , viewWeekOfGames model games
                ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewLoadError : String -> Html Msg
viewLoadError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    errorText []
        [ emphasisText [] [ text errorHeading ]
        , text <| "Error: " ++ errorMessage
        ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage message =
    case message of
        Just m ->
            errorText [] [ text <| "Error: " ++ m ]

        Nothing ->
            text ""



{- View Header -}


viewDivisionHeader : Division -> Session -> Html Msg
viewDivisionHeader division session =
    row []
        [ mainHeader [] [ text division.name ]
        , emphasisText [] [ text <| "Season " ++ String.fromInt division.season ]
        , if division.closed then
            text ""

          else
            requiresAuth session viewAddTeamButton
        ]


viewAddTeamButton : Html Msg
viewAddTeamButton =
    addButton
        [ floatRight, onClick AddTeamButtonClick ]
        [ text "Add Team to Div" ]



{- View Teams Table -}


viewStandings : Model -> Bool -> List Standing -> Html Msg
viewStandings model divClosed standings =
    table []
        [ viewTableHeader divClosed model.session model.sortingMethod
        , tableBody [] <|
            List.map (viewStandingTableRow model.session divClosed model.games) <|
                sortedStandings model.sortingMethod standings
        ]


viewTableHeader : Bool -> Session -> TeamSortingMethod -> Html Msg
viewTableHeader divClosed session sortMethod =
    tableHead []
        [ ( [ onClick TeamNameSortClick ]
          , [ case sortMethod of
                Name ->
                    text "Name ▲"

                NameDesc ->
                    text "Name ▼"

                _ ->
                    text "Name"
            ]
          )
        , ( [ onClick TeamRaceSortClick ]
          , [ case sortMethod of
                Race ->
                    text "Race ▲"

                RaceDesc ->
                    text "Race ▼"

                _ ->
                    text "Race"
            ]
          )
        , ( [ onClick TeamCoachSortClick ]
          , [ case sortMethod of
                Coach ->
                    text "Coach ▲"

                CoachDesc ->
                    text "Coach ▼"

                _ ->
                    text "Coach"
            ]
          )
        , ( [ onClick TeamEloSortClick ]
          , [ case sortMethod of
                Elo ->
                    text "Elo ▲"

                EloDesc ->
                    text "Elo ▼"

                _ ->
                    text "Elo"
            ]
          )
        , ( [ onClick DefaultSortClick ], [ text "Points" ] )
        , ( [ onClick DefaultSortClick ], [ text "Games" ] )
        , ( [ onClick DefaultSortClick ], [ requiresAuth session <| text "Total" ] )
        , ( [ onClick DefaultSortClick ], [ text "W-D-L" ] )
        , ( [ onClick DefaultSortClick ], [ text "TDD" ] )
        , ( []
          , [ if divClosed then
                text ""

              else
                text "Strength of Schedule"
            ]
          )
        , ( [ onClick DefaultSortClick ], [ requiresAuth session <| text " " ] )
        ]


viewStandingTableRow : Session -> Bool -> WebData (List Game) -> Standing -> Html Msg
viewStandingTableRow session divClosed games standing =
    tableRow []
        [ ( []
          , [ pageLink
                [ onClick <| ViewTeamClick standing.team.id ]
                [ text standing.team.name ]
            , viewAccolades standing.team.accolades
            ]
          )
        , ( [], [ text standing.team.race.name ] )
        , ( []
          , [ pageLink
                [ onClick <| ViewCoachClick standing.team.coach.id ]
                [ text standing.team.coach.name ]
            , viewAccolades standing.team.coach.accolades
            ]
          )
        , ( [], [ text <| String.fromInt standing.team.elo ] )
        , ( [], [ text <| String.fromInt <| getPoints standing ] )
        , ( [], [ text <| String.fromInt <| getGamesPlayed standing ] )
        , ( [], [ requiresAuth session <| text <| String.fromInt <| getTotalGames games standing.team ] )
        , ( [], [ text <| String.fromInt standing.wins ++ " - " ++ String.fromInt standing.draws ++ " - " ++ String.fromInt standing.losses ] )
        , ( [], [ text <| String.fromInt <| getTDD standing ] )
        , ( []
          , [ if divClosed then
                text ""

              else
                text (Maybe.andThen (String.fromInt >> Just) standing.avgRemainingElo |> Maybe.withDefault "No games remaining")
            ]
          )
        , ( [], [ requiresAuth session <| viewTeamEditButton standing.team, requiresAuth session <| viewTeamDeleteButton standing.team ] )
        ]


getTotalGames : WebData (List Game) -> Team -> Int
getTotalGames gamesData team =
    case gamesData of
        RemoteData.Success games ->
            List.filter (\g -> (g.homeTeam.id == team.id) || (g.awayTeam.id == team.id)) games
                |> List.length

        _ ->
            0


viewAccolades : List Accolade -> Html Msg
viewAccolades accolades =
    accoladeCollection []
        (List.sortWith (\a b -> compare (Maybe.withDefault 0 b.season) (Maybe.withDefault 0 a.season)) accolades
            |> List.take 3
            |> List.map viewAccolade
        )


viewTeamDeleteButton : Team -> Html Msg
viewTeamDeleteButton team =
    warnButton
        [ onClick (DeleteTeamButtonClick team.id) ]
        [ text "Delete" ]


viewTeamEditButton : Team -> Html Msg
viewTeamEditButton team =
    optionButton
        [ onClick (EditTeamButtonClick team.id) ]
        [ text "Edit" ]



{- View Games -}


viewGamesHeader : Division -> List Game -> Session -> Html Msg
viewGamesHeader division games session =
    row []
        [ subHeader [] [ text "Scheduled Games" ]
        , if division.closed then
            text ""

          else
            requiresAuth session <| viewAddWeekButton <| maxWeek games + 1
        ]


viewAddWeekButton : Int -> Html Msg
viewAddWeekButton nextWeek =
    addButton
        [ onClick <| AddWeekButtonClick nextWeek
        , floatRight
        ]
        [ text <| "Add Week " ++ String.fromInt nextWeek ]


viewWeekOfGames : Model -> List Game -> Html Msg
viewWeekOfGames model games =
    shadedContainer []
        (viewWeekTitle model.displayedWeek
            :: (gamesInWeek model.displayedWeek games
                    |> List.map (viewGame model.session)
               )
            ++ [ requiresAuth model.session <| viewAddGameButton model.displayedWeek
               , viewPageSelect model.displayedWeek games
               ]
        )


viewWeekTitle : Int -> Html Msg
viewWeekTitle currWeek =
    h2 [ floatCenter ] [ text <| "Week " ++ String.fromInt currWeek ]


viewGame : Session -> Game -> Html Msg
viewGame session game =
    floatingCard []
        [ viewGameDetails game
        , requiresAuth session <| viewGameButtons game
        ]


viewGameDetails : Game -> Html Msg
viewGameDetails game =
    narrowRow []
        [ colThird [ textRight ]
            [ smallColorText [] [ text game.homeTeam.name ]
            , bodyText [] [ text game.homeTeam.coach.name ]
            ]
        , colThird [ textCenter ] [ viewScore game, viewOdds game ]
        , colThird [ textLeft ]
            [ smallColorText [] [ text game.awayTeam.name ]
            , bodyText [] [ text game.awayTeam.coach.name ]
            ]
        ]


viewOdds : Game -> Html Msg
viewOdds game =
    case game.homeOdds of
        Just homeOdds ->
            case game.awayOdds of
                Just awayOdds ->
                    bodyText [ floatCenter ] [ text <| Game.oddsToString homeOdds ++ " - " ++ Game.oddsToString awayOdds ]

                Nothing ->
                    text ""

        Nothing ->
            text ""


viewScore : Game -> Html Msg
viewScore game =
    case game.homeScore of
        Just homeScore ->
            case game.awayScore of
                Just awayScore ->
                    emphasisText [ floatCenter ] [ text <| String.fromInt homeScore ++ " - " ++ String.fromInt awayScore ]

                Nothing ->
                    emphasisText [ floatCenter ] [ text "vs." ]

        Nothing ->
            emphasisText [ floatCenter ] [ text "vs." ]


viewGameButtons : Game -> Html Msg
viewGameButtons game =
    narrowRow [ floatCenter ]
        [ viewGameEditButton game
        , viewGameDeleteButton game
        ]


viewGameDeleteButton : Game -> Html Msg
viewGameDeleteButton game =
    warnButton
        [ onClick (DeleteGameButtonClick game.id) ]
        [ text "Delete" ]


viewGameEditButton : Game -> Html Msg
viewGameEditButton game =
    optionButton
        [ onClick (EditGameButtonClick game.id) ]
        [ text "Edit" ]


viewAddGameButton : Int -> Html Msg
viewAddGameButton week =
    row [ floatCenter ]
        [ addButton
            [ onClick <| AddGameButtonClick week ]
            [ text "Add Game" ]
        ]



-- Pagination --


viewPageSelect : Int -> List Game -> Html Msg
viewPageSelect week games =
    pageBar []
        [ pageBarButton [ onClick FirstPageClick ] [ text "<<" ]
        , pageBarButton [ onClick PrevPageClick ] [ text "<" ]
        , pageBarFiller [] [ text <| String.fromInt week ++ " of " ++ String.fromInt (maxWeek games) ]
        , pageBarButton [ onClick NextPageClick ] [ text ">" ]
        , pageBarButton [ onClick LastPageClick ] [ text ">>" ]
        ]
