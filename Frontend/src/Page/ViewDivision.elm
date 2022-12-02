module Page.ViewDivision exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Attributes
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Division exposing (Division, DivisionId, divisionDecoder)
import Model.Game as Game exposing (Game, GameId, gamesDecoder)
import Model.Session exposing (Session)
import Model.Team exposing (Team, TeamId, teamsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)
import Url exposing (Protocol(..))



-- Types --


type alias Model =
    { teams : WebData (List Team)
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
    | TeamsReceived (WebData (List Team))
    | GamesReceived (WebData (List Game))
    | DivisionReceived (WebData Division)
    | AddTeamButtonClick
    | DeleteTeamButtonClick TeamId
    | EditTeamButtonClick TeamId
    | TeamDeleted (Result Http.Error DeleteResponse)
    | GameDeleted (Result Http.Error DeleteResponse)
    | TeamNameSortClick
    | TeamRaceSortClick
    | TeamCoachSortClick
    | TeamEloSortClick
    | ChangeWeek Int
    | DeleteGameButtonClick GameId
    | EditGameButtonClick GameId
    | AddGameButtonClick Int
    | AddWeekButtonClick Int


type TeamSortingMethod
    = None
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
    ( { teams = RemoteData.Loading
      , sortingMethod = None
      , division = RemoteData.Loading
      , games = RemoteData.Loading
      , displayedWeek = Maybe.withDefault 1 startWeek
      , session = session
      , deleteError = Nothing
      , divisionId = id
      }
    , Cmd.batch
        [ getTeamsInDivRequest session.token id
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
                | teams = RemoteData.Loading
                , division = RemoteData.Loading
              }
            , Cmd.batch
                [ getTeamsInDivRequest model.session.token model.divisionId
                , getDivisionRequest model.session.token model.divisionId
                , getGamesRequest model.session.token model.divisionId
                ]
            )

        TeamsReceived response ->
            ( { model | teams = response }, Cmd.none )

        GamesReceived response ->
            ( { model | games = response }, Cmd.none )

        DivisionReceived response ->
            ( { model | division = response }, Cmd.none )

        AddTeamButtonClick ->
            ( model, pushUrl model.session.navkey <| Route.AddTeamToDivision model.divisionId )

        EditTeamButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.EditTeam id )

        DeleteTeamButtonClick id ->
            ( model, deleteTeamRequest model.session.token id )

        TeamDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getTeamsInDivRequest model.session.token model.divisionId )

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

        TeamEloSortClick ->
            ( { model | sortingMethod = newSort Elo EloDesc model.sortingMethod }, Cmd.none )

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



-- API Requests --


getTeamsInDivRequest : Maybe String -> DivisionId -> Cmd Msg
getTeamsInDivRequest token divId =
    Api.getRequest token (Api.TeamsInDiv divId) <|
        Http.expectJson (RemoteData.fromResult >> TeamsReceived) teamsDecoder


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


sortedTeams : TeamSortingMethod -> List Team -> List Team
sortedTeams sortingMethod teams =
    case sortingMethod of
        None ->
            teams

        Name ->
            List.sortWith (\a b -> compare a.name b.name) teams

        NameDesc ->
            List.sortWith (\a b -> compare b.name a.name) teams

        Coach ->
            List.sortWith (\a b -> compare a.coach.name b.coach.name) teams

        CoachDesc ->
            List.sortWith (\a b -> compare b.coach.name a.coach.name) teams

        Race ->
            List.sortWith (\a b -> compare a.race.name b.race.name) teams

        RaceDesc ->
            List.sortWith (\a b -> compare b.race.name a.race.name) teams

        Elo ->
            List.sortWith (\a b -> compare a.elo b.elo) teams

        EloDesc ->
            List.sortWith (\a b -> compare b.elo a.elo) teams


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
    div []
        [ div Custom.Attributes.row [ viewRefreshButton ]
        , viewErrorMessage model.deleteError
        , viewTeamsOrError model
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    div [ Custom.Attributes.col ]
        [ button
            [ onClick RefreshButtonClick
            , Custom.Attributes.refreshButton
            ]
            [ text "Refresh Page" ]
        ]



{- View ErrorHandling -}


viewTeamsOrError : Model -> Html Msg
viewTeamsOrError model =
    case model.teams of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError

        RemoteData.Success teams ->
            case model.division of
                RemoteData.Success division ->
                    div []
                        [ viewDivisionHeader division model.session
                        , viewTeams model teams
                        , viewGamesOrError model division
                        ]

                RemoteData.NotAsked ->
                    text ""

                RemoteData.Loading ->
                    h3 [] [ text "Loading..." ]

                RemoteData.Failure httpError ->
                    viewLoadError <| Error.buildErrorMessage httpError


viewGamesOrError : Model -> Division -> Html Msg
viewGamesOrError model division =
    case model.games of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success games ->
            div []
                [ br [] []
                , viewGamesHeader division games model.session
                , br [] []
                , viewGamesCarousel model division games
                ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


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


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage message =
    case message of
        Just m ->
            div [ Custom.Attributes.errorMessage ]
                [ text <| "Error: " ++ m ]

        Nothing ->
            text ""



{- View Header -}


viewDivisionHeader : Division -> Session -> Html Msg
viewDivisionHeader division session =
    div Custom.Attributes.row
        [ div [ Custom.Attributes.col ]
            [ h3 [] [ text division.name ]
            , h6 [] [ text <| "Season " ++ String.fromInt division.season ]
            ]
        , if division.closed then
            text ""

          else
            div [ Custom.Attributes.col ] [ requiresAuth session viewAddTeamButton ]
        ]


viewAddTeamButton : Html Msg
viewAddTeamButton =
    div [ Custom.Attributes.rightSideButtons ]
        [ button
            [ Custom.Attributes.addButton
            , onClick AddTeamButtonClick
            ]
            [ text "Add Team to Div" ]
        ]



{- View Teams Table -}


viewTeams : Model -> List Team -> Html Msg
viewTeams model teams =
    table [ Custom.Attributes.table ]
        [ viewTableHeader model.sortingMethod
        , tbody [] <|
            List.map (viewTeamTableRow model.session) <|
                sortedTeams model.sortingMethod teams
        ]


viewTableHeader : TeamSortingMethod -> Html Msg
viewTableHeader sortMethod =
    thead []
        [ tr []
            [ th [ scope "col", onClick TeamNameSortClick ]
                [ case sortMethod of
                    Name ->
                        text "Name ▲"

                    NameDesc ->
                        text "Name ▼"

                    _ ->
                        text "Name"
                ]
            , th [ scope "col", onClick TeamRaceSortClick ]
                [ case sortMethod of
                    Race ->
                        text "Race ▲"

                    RaceDesc ->
                        text "Race ▼"

                    _ ->
                        text "Race"
                ]
            , th [ scope "col", onClick TeamCoachSortClick ]
                [ case sortMethod of
                    Coach ->
                        text "Coach ▲"

                    CoachDesc ->
                        text "Coach ▼"

                    _ ->
                        text "Coach"
                ]
            , th [ scope "col", onClick TeamEloSortClick ]
                [ case sortMethod of
                    Elo ->
                        text "Elo ▲"

                    EloDesc ->
                        text "Elo ▼"

                    _ ->
                        text "Elo"
                ]
            , th [ scope "col" ]
                [ text "" ]
            ]
        ]


viewTeamTableRow : Session -> Team -> Html Msg
viewTeamTableRow session team =
    tr []
        [ td []
            [ text team.name ]
        , td []
            [ text team.race.name ]
        , td []
            [ text team.coach.name ]
        , td []
            [ text <| String.fromInt team.elo ]
        , requiresAuth session <|
            td (Custom.Attributes.tableButtonColumn 2)
                [ viewTeamEditButton team, viewTeamDeleteButton team ]
        ]


viewTeamDeleteButton : Team -> Html Msg
viewTeamDeleteButton team =
    button
        (onClick (DeleteTeamButtonClick team.id) :: Custom.Attributes.deleteButton)
        [ text "Delete" ]


viewTeamEditButton : Team -> Html Msg
viewTeamEditButton team =
    button
        (onClick (EditTeamButtonClick team.id) :: Custom.Attributes.editButton)
        [ text "Edit" ]



{- View Games -}


viewGamesHeader : Division -> List Game -> Session -> Html Msg
viewGamesHeader division games session =
    div Custom.Attributes.row
        [ div [ Custom.Attributes.col ]
            [ h3 [ id "week" ] [ text "Scheduled Games" ]
            ]
        , if division.closed then
            text ""

          else
            div [ Custom.Attributes.col ] [ requiresAuth session <| viewAddWeekButton <| maxWeek games + 1 ]
        ]


viewAddWeekButton : Int -> Html Msg
viewAddWeekButton nextWeek =
    div [ Custom.Attributes.rightSideButtons ]
        [ button
            [ Custom.Attributes.addButton
            , onClick <| AddWeekButtonClick nextWeek
            ]
            [ text <| "Add Week " ++ String.fromInt nextWeek ]
        ]


viewGamesCarousel : Model -> Division -> List Game -> Html Msg
viewGamesCarousel model division games =
    let
        thisId =
            "games"

        endWeek =
            maxWeek games
    in
    div
        (id thisId :: Custom.Attributes.carouselContainer)
        [ carouselIndicators thisId endWeek model.displayedWeek
        , viewGames division model.session games endWeek model.displayedWeek
        , carouselPrev model.displayedWeek
        , carouselNext model.displayedWeek endWeek
        ]


carouselIndicators : String -> Int -> Int -> Html Msg
carouselIndicators id endWeek currPage =
    ol [ Custom.Attributes.carouselIndicators ]
        (List.range 1 endWeek
            |> List.map (\week -> carouselIndicator id week currPage)
        )


carouselIndicator : String -> Int -> Int -> Html Msg
carouselIndicator id week currWeek =
    button
        [ Custom.Attributes.button
        , Custom.Attributes.dataBsTarget <| "#" ++ id
        , onClick <| ChangeWeek week
        , if week == currWeek then
            class "active"

          else
            class ""
        ]
        []


carouselPrev : Int -> Html Msg
carouselPrev currWeek =
    button
        [ Custom.Attributes.carouselPrevButton
        , onClick <| ChangeWeek <| Basics.max 1 (currWeek - 1)
        ]
        [ span [ Custom.Attributes.carouselPrevIcon ] []
        , span [ Custom.Attributes.visuallyHidden ] [ text "Previous" ]
        ]


carouselNext : Int -> Int -> Html Msg
carouselNext currWeek endWeek =
    button
        [ Custom.Attributes.carouselNextButton
        , onClick <| ChangeWeek <| Basics.min endWeek (currWeek + 1)
        ]
        [ span [ Custom.Attributes.carouselNextIcon ] []
        , span [ Custom.Attributes.visuallyHidden ] [ text "Next" ]
        ]


viewGames : Division -> Session -> List Game -> Int -> Int -> Html Msg
viewGames division session games endWeek currWeek =
    div [ Custom.Attributes.carouselInner ]
        (List.range 1 endWeek
            |> List.map (\week -> viewWeek division session games week currWeek)
        )


viewWeek : Division -> Session -> List Game -> Int -> Int -> Html Msg
viewWeek division session games thisWeek currWeek =
    div
        (if thisWeek == currWeek then
            class "active" :: Custom.Attributes.carouselItem

         else
            Custom.Attributes.carouselItem
        )
    <|
        List.append
            (viewWeekTitle thisWeek
                :: (List.map (viewGame session) <| gamesInWeek thisWeek games)
            )
            [ if division.closed then
                text ""

              else
                requiresAuth session <| viewAddGameButton thisWeek
            ]


viewWeekTitle : Int -> Html msg
viewWeekTitle currWeek =
    div
        [ Custom.Attributes.textCentered ]
        [ h5 [] [ text <| "Week " ++ String.fromInt currWeek ]
        , br [] []
        ]


viewGame : Session -> Game -> Html Msg
viewGame session game =
    div
        Custom.Attributes.carouselItemEntry
        [ p [] [ text <| game.homeTeam.name ++ " vs. " ++ game.awayTeam.name ]
        , viewOdds game
        , viewScore game
        , requiresAuth session <| viewGameButtons game
        ]


viewOdds : Game -> Html Msg
viewOdds game =
    case game.homeOdds of
        Just homeOdds ->
            case game.awayOdds of
                Just awayOdds ->
                    p [] [ text <| Game.oddsToString homeOdds ++ " - " ++ Game.oddsToString awayOdds ]

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
                    p [] [ text <| String.fromInt homeScore ++ " - " ++ String.fromInt awayScore ]

                Nothing ->
                    text ""

        Nothing ->
            text ""


viewGameButtons : Game -> Html Msg
viewGameButtons game =
    div []
        [ viewGameEditButton game
        , viewGameDeleteButton game
        ]


viewGameDeleteButton : Game -> Html Msg
viewGameDeleteButton game =
    button
        (onClick (DeleteGameButtonClick game.id) :: Custom.Attributes.deleteButton)
        [ text "Delete" ]


viewGameEditButton : Game -> Html Msg
viewGameEditButton game =
    button
        (onClick (EditGameButtonClick game.id) :: Custom.Attributes.editButton)
        [ text "Edit" ]


viewAddGameButton : Int -> Html Msg
viewAddGameButton week =
    div [ Custom.Attributes.textCentered ]
        [ button
            [ Custom.Attributes.addButton
            , Custom.Attributes.centered
            , onClick <| AddGameButtonClick week
            ]
            [ text "Add Game" ]
        ]
