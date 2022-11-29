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
import Model.Game exposing (Game, gamesDecoder)
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
    | TeamNameSortClick
    | TeamRaceSortClick
    | TeamCoachSortClick
    | TeamEloSortClick
    | ChangeWeek Int


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


init : Session -> DivisionId -> ( Model, Cmd Msg )
init session id =
    ( { teams = RemoteData.Loading
      , sortingMethod = None
      , division = RemoteData.Loading
      , games = RemoteData.Loading
      , displayedWeek = 1
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

        RemoteData.Success teams ->
            div []
                [ viewHeaderOrError model.division model.session
                , viewTeams model teams
                , viewGamesOrError model
                ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewGamesOrError : Model -> Html Msg
viewGamesOrError model =
    case model.games of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success games ->
            div []
                [ br [] []
                , h3 [] [ text "Scheduled Matches" ]
                , viewGamesCarousel model games
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


viewHeaderOrError : WebData Division -> Session -> Html Msg
viewHeaderOrError data session =
    case data of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            viewHeader "Loading..." "" session

        RemoteData.Success division ->
            viewHeader division.name ("Season " ++ String.fromInt division.season) session

        RemoteData.Failure httpError ->
            viewHeader (Error.buildErrorMessage httpError) "" session



{- View Header -}


viewHeader : String -> String -> Session -> Html Msg
viewHeader title subtitle session =
    div Custom.Attributes.row
        [ div [ Custom.Attributes.col ]
            [ h3 [] [ text title ]
            , h6 [] [ text subtitle ]
            ]
        , div [ Custom.Attributes.col ] [ requiresAuth session viewToolBar ]
        ]


viewToolBar : Html Msg
viewToolBar =
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
            td [ Custom.Attributes.tableButtonColumn 2 ]
                [ viewEditButton team, viewDeleteButton team ]
        ]


viewDeleteButton : Team -> Html Msg
viewDeleteButton team =
    button
        (onClick (DeleteTeamButtonClick team.id) :: Custom.Attributes.deleteButton)
        [ text "Delete" ]


viewEditButton : Team -> Html Msg
viewEditButton team =
    button
        (onClick (EditTeamButtonClick team.id) :: Custom.Attributes.editButton)
        [ text "Edit" ]



{- View Games Table -}


viewGamesCarousel : Model -> List Game -> Html Msg
viewGamesCarousel model games =
    let
        thisId =
            "gamesCarousel"

        endWeek =
            maxWeek games
    in
    div
        [ id thisId
        , class "carousel slide"
        , attribute "data-ride" "carousel"
        , attribute "data-bs-interval" "false"
        , attribute "data-bs-wrap" "false"
        , style "background-color" "#ddd"
        ]
        [ carouselIndicators thisId endWeek model.displayedWeek
        , viewGames games endWeek model.displayedWeek
        , carouselPrev thisId model.displayedWeek
        , carouselNext thisId model.displayedWeek endWeek
        ]


carouselIndicators : String -> Int -> Int -> Html Msg
carouselIndicators id endWeek currPage =
    ol [ class "carousel-indicators" ]
        (List.range 1 endWeek
            |> List.map (\week -> carouselIndicator id week currPage)
        )


carouselIndicator : String -> Int -> Int -> Html Msg
carouselIndicator id week currWeek =
    button
        [ type_ "button"
        , attribute "data-bs-target" <| "#" ++ id
        , attribute "data-bs-slide-to" <| String.fromInt week
        , onClick <| ChangeWeek week
        , if week == currWeek then
            class "active"

          else
            class ""
        ]
        []


carouselPrev : String -> Int -> Html Msg
carouselPrev id currWeek =
    a
        [ class "carousel-control-prev"
        , href <| "#" ++ id
        , onClick <| ChangeWeek <| Basics.max 1 (currWeek - 1)
        ]
        [ span [ class "carousel-control-prev-icon" ] []
        , span [ class "visually-hidden" ] [ text "Previous" ]
        ]


carouselNext : String -> Int -> Int -> Html Msg
carouselNext id currWeek endWeek =
    a
        [ class "carousel-control-next"
        , href <| "#" ++ id
        , onClick <| ChangeWeek <| Basics.min endWeek (currWeek + 1)
        ]
        [ span [ class "carousel-control-next-icon" ] []
        , span [ class "visually-hidden" ] [ text "Next" ]
        ]


viewGames : List Game -> Int -> Int -> Html Msg
viewGames games endWeek currWeek =
    div [ class "carousel-inner" ]
        (List.range 1 endWeek
            |> List.map (\week -> viewWeek games week currWeek)
        )


viewWeek : List Game -> Int -> Int -> Html Msg
viewWeek games thisWeek currWeek =
    div
        [ class "carousel-item"
        , if thisWeek == currWeek then
            class "active"

          else
            class ""
        , style "padding" "3em 5em 5em"
        ]
    <|
        viewWeekTitle thisWeek
            :: (List.map viewGame <| gamesInWeek thisWeek games)


viewWeekTitle : Int -> Html msg
viewWeekTitle currWeek =
    div
        [ style "text-align" "center" ]
        [ h5 [] [ text <| "Week " ++ String.fromInt currWeek ]
        , br [] []
        ]


viewGame : Game -> Html Msg
viewGame game =
    div
        [ style "text-align" "center" ]
        [ text <| game.homeTeam.name ++ " vs. " ++ game.awayTeam.name ]
