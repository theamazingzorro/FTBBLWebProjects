module Page.ViewTeam exposing (Model, Msg, init, update, view)

import Api exposing (Endpoint(..))
import Auth exposing (requiresAuth)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Http
import LineChart
import Model.Accolade exposing (Accolade, viewAccolade)
import Model.Coach exposing (CoachId)
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.DivStanding exposing (DivStanding, divStandingsDecoder, getTDD)
import Model.Division exposing (Division, DivisionId)
import Model.EloHistory exposing (EloHistory, historyListDecoder, maxElo)
import Model.Session exposing (Session)
import Model.Team exposing (Team, TeamId, teamDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Model --


type alias Model =
    { session : Session
    , id : TeamId
    , team : WebData Team
    , standingsHistory : WebData (List DivStanding)
    , teamHistory : WebData (List EloHistory)
    , deleteError : Maybe String
    }


type Msg
    = TeamReceived (WebData Team)
    | HistoryReceived (WebData (List EloHistory))
    | StandingsReceived (WebData (List DivStanding))
    | ViewDivisionButtonClick DivisionId
    | ViewCoachClick CoachId
    | DeleteTeamButtonClick
    | EditTeamButtonClick
    | AddAccoladeButtonClick TeamId CoachId
    | TeamDeleted (Result Http.Error DeleteResponse)



-- Init --


init : Session -> TeamId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , team = RemoteData.Loading
      , standingsHistory = RemoteData.Loading
      , teamHistory = RemoteData.Loading
      , deleteError = Nothing
      }
    , Cmd.batch
        [ getTeamRequest session.token id
        , getTeamHistoryRequest session.token id
        , getStandingsHistoryRequest session.token id
        ]
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamReceived response ->
            ( { model | team = response }, Cmd.none )

        HistoryReceived response ->
            ( { model | teamHistory = response }, Cmd.none )

        StandingsReceived response ->
            ( { model | standingsHistory = sortStandingsChron response }, Cmd.none )

        ViewDivisionButtonClick divId ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision divId )

        ViewCoachClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewCoach id )

        AddAccoladeButtonClick teamId coachId ->
            ( model, pushUrl model.session.navkey <| Route.AddAccoladeWithDefaults (Just teamId) coachId )

        EditTeamButtonClick ->
            ( model, pushUrl model.session.navkey <| Route.EditTeam model.id )

        DeleteTeamButtonClick ->
            ( model, deleteTeamRequest model.session.token model.id )

        TeamDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }
            , if res.deleted then
                pushUrl model.session.navkey Route.Teams

              else
                Cmd.none
            )

        TeamDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )


sortStandingsChron : WebData (List DivStanding) -> WebData (List DivStanding)
sortStandingsChron data =
    case data of
        RemoteData.Success standings ->
            RemoteData.Success <| List.reverse <| List.sortBy (.div >> .season) standings

        other ->
            other


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. Team not found."



-- API Requests --


getTeamRequest : Maybe String -> TeamId -> Cmd Msg
getTeamRequest token id =
    Api.getRequest token (Api.Team id) <|
        Http.expectJson (RemoteData.fromResult >> TeamReceived) teamDecoder


getTeamHistoryRequest : Maybe String -> TeamId -> Cmd Msg
getTeamHistoryRequest token id =
    Api.getRequest token (Api.TeamEloHistory id) <|
        Http.expectJson (RemoteData.fromResult >> HistoryReceived) historyListDecoder


getStandingsHistoryRequest : Maybe String -> TeamId -> Cmd Msg
getStandingsHistoryRequest token id =
    Api.getRequest token (Api.DivStandings id) <|
        Http.expectJson (RemoteData.fromResult >> StandingsReceived) divStandingsDecoder


deleteTeamRequest : Maybe String -> TeamId -> Cmd Msg
deleteTeamRequest token id =
    Api.deleteRequest token (Api.Team id) <|
        Http.expectJson TeamDeleted deleteResponseDecoder



-- View --


view : Model -> Html Msg
view model =
    case model.team of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success team ->
            row []
                [ viewErrorMessage model.deleteError
                , viewTeam model team
                ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


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


viewToolBar : Team -> Html Msg
viewToolBar team =
    row [ floatRight ]
        [ addButton
            [ onClick <| AddAccoladeButtonClick team.id team.coach.id ]
            [ text "Add Accolade" ]
        , optionButton
            [ onClick EditTeamButtonClick ]
            [ text "Edit" ]
        , warnButton
            [ onClick DeleteTeamButtonClick ]
            [ text "Delete" ]
        ]


viewTeam : Model -> Team -> Html Msg
viewTeam model team =
    div []
        [ viewTeamDetails model team
        , viewResultsHistory model
        , viewTeamEloHistory model
        ]


viewResultsHistory : Model -> Html Msg
viewResultsHistory model =
    case model.standingsHistory of
        RemoteData.Success history ->
            viewPastResultsTable history

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading Match History..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewTeamEloHistory : Model -> Html Msg
viewTeamEloHistory model =
    case model.teamHistory of
        RemoteData.Success history ->
            viewTeamEloGraph history

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading Elo History..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewTeamDetails : Model -> Team -> Html Msg
viewTeamDetails model team =
    row []
        [ colTwoThird []
            [ mainHeader [] [ text team.name ]
            , bodyText []
                [ text "Coach: "
                , pageLink
                    [ onClick <| ViewCoachClick team.coach.id ]
                    [ text team.coach.name ]
                ]
            , bodyText [] [ text <| "Race: " ++ team.race.name ]
            , bodyText [] [ text <| "Current Elo: " ++ String.fromInt team.elo ]
            , bodyText [] [ text <| "Max Elo: " ++ viewMaxElo model.teamHistory ]
            , bodyText [] [ text "Most Recent Division: ", Maybe.map viewDivision team.division |> Maybe.withDefault (text "N/A") ]
            ]
        , colThird []
            (if team.accolades /= [] then
                [ viewAccolades team
                , requiresAuth model.session <| viewToolBar team
                ]

             else
                [ requiresAuth model.session <| viewToolBar team ]
            )
        ]


viewMaxElo : WebData (List EloHistory) -> String
viewMaxElo historyData =
    case historyData of
        RemoteData.Success history ->
            String.fromInt <| maxElo history

        RemoteData.NotAsked ->
            ""

        RemoteData.Loading ->
            "Loading Elo History..."

        RemoteData.Failure httpError ->
            Error.buildErrorMessage httpError


viewAccolades : Team -> Html Msg
viewAccolades team =
    table []
        [ tableHead []
            [ ( [], [ text "" ] )
            , ( [], [ text "Achievements" ] )
            ]
        , tableBody [] <|
            List.map viewAccoladeRow team.accolades
        ]


viewAccoladeRow : Accolade -> Html Msg
viewAccoladeRow accolade =
    tableRow []
        [ ( [], [ viewAccolade accolade ] )
        , ( [], [ text <| accolade.name ++ (Maybe.map (\season -> " Season " ++ String.fromInt season) accolade.season |> Maybe.withDefault "") ] )
        ]


viewPastResultsTable : List DivStanding -> Html Msg
viewPastResultsTable standings =
    div []
        [ subHeader [] [ text "Past Results" ]
        , table []
            [ viewTableHeader
            , tableBody [] <|
                List.map viewTableRow standings
            ]
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tableHead []
        [ ( [], [ text "Div" ] )
        , ( [], [ text "Rank" ] )
        , ( [], [ text "W-D-L" ] )
        , ( [], [ text "TDD" ] )
        ]


viewTableRow : DivStanding -> Html Msg
viewTableRow standing =
    tableRow []
        [ ( [], [ viewDivision standing.div ] )
        , ( [], [ text <| String.fromInt standing.rank ] )
        , ( [], [ text <| String.fromInt standing.wins ++ " - " ++ String.fromInt standing.draws ++ " - " ++ String.fromInt standing.losses ] )
        , ( [], [ text <| String.fromInt <| getTDD standing ] )
        ]


viewDivision : Division -> Html Msg
viewDivision division =
    pageLink
        [ onClick <| ViewDivisionButtonClick division.id ]
        [ text <| division.name ++ " Season " ++ String.fromInt division.season ]


viewTeamEloGraph : List EloHistory -> Html Msg
viewTeamEloGraph history =
    row []
        [ subHeader [] [ text "Elo History" ]
        , List.map (\h -> ( h.date, toFloat h.elo )) history
            |> LineChart.viewChart
        ]
