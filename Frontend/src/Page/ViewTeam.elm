module Page.ViewTeam exposing (Model, Msg, init, update, view)

import Api exposing (Endpoint(..))
import Custom.Attributes
import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import LineChart
import Model.Accolade exposing (Accolade, viewAccolade)
import Model.DivStanding exposing (DivStanding, divStandingsDecoder, getTDD)
import Model.Division exposing (Division, DivisionId)
import Model.EloHistory exposing (EloHistory, historyListDecoder)
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
    }


type Msg
    = TeamReceived (WebData Team)
    | HistoryReceived (WebData (List EloHistory))
    | StandingsReceived (WebData (List DivStanding))
    | ViewDivisionButtonClick DivisionId



-- Init --


init : Session -> TeamId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , team = RemoteData.Loading
      , standingsHistory = RemoteData.Loading
      , teamHistory = RemoteData.Loading
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
            ( { model | standingsHistory = response }, Cmd.none )

        ViewDivisionButtonClick divId ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision divId )



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



-- View --


view : Model -> Html Msg
view model =
    case model.team of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success team ->
            viewTeam model team

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


viewTeam : Model -> Team -> Html Msg
viewTeam model team =
    div []
        [ br [] []
        , viewTeamDetails team
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
            h3 [] [ text "Loading Match History..." ]

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
            h3 [] [ text "Loading Elo History..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewTeamDetails : Team -> Html Msg
viewTeamDetails team =
    div [ class "row" ]
        [ div [ class " col" ]
            [ h3 [] [ text team.name ]
            , br [] []
            , p [] [ text <| "Coach: " ++ team.coach.name ]
            , p [] [ text <| "Race: " ++ team.race.name ]
            , p [] [ text <| "Current Elo: " ++ String.fromInt team.elo ]
            , p [] [ text "Most Recent Division: ", Maybe.map viewDivision team.division |> Maybe.withDefault (text "N/A") ]
            ]
        , div [ class "col" ]
            [ if team.accolades /= [] then
                viewAccolades team

              else
                text ""
            ]
        ]


viewAccolades : Team -> Html Msg
viewAccolades team =
    table [ Custom.Attributes.table ]
        [ thead []
            [ tr []
                [ th [ scope "col" ]
                    [ text "" ]
                , th [ scope "col" ]
                    [ text "Achievements" ]
                ]
            ]
        , tbody [] <|
            List.map viewAccoladeRow team.accolades
        ]


viewAccoladeRow : Accolade -> Html Msg
viewAccoladeRow accolade =
    tr []
        [ td []
            [ viewAccolade accolade ]
        , td []
            [ text <| accolade.name ++ (Maybe.map (\season -> " Season " ++ String.fromInt season) accolade.season |> Maybe.withDefault "") ]
        ]


viewPastResultsTable : List DivStanding -> Html Msg
viewPastResultsTable standings =
    div []
        [ br [] []
        , br [] []
        , h4 [] [ text "Past Results" ]
        , table [ Custom.Attributes.table ]
            [ viewTableHeader
            , tbody [] <|
                List.map viewTableRow standings
            ]
        ]


viewTableHeader : Html Msg
viewTableHeader =
    thead []
        [ tr []
            [ th [ scope "col" ]
                [ text "Div" ]
            , th [ scope "col", Custom.Attributes.textCentered ]
                [ text "Rank" ]
            , th [ scope "col", Custom.Attributes.textCentered ]
                [ text "W-D-L" ]
            , th [ scope "col", Custom.Attributes.textCentered ]
                [ text "TDD" ]
            ]
        ]


viewTableRow : DivStanding -> Html Msg
viewTableRow standing =
    tr []
        [ td []
            [ viewDivision standing.div ]
        , td [ Custom.Attributes.textCentered ]
            [ text <| String.fromInt standing.rank ]
        , td [ Custom.Attributes.textCentered ]
            [ text <| String.fromInt standing.wins ++ " - " ++ String.fromInt standing.draws ++ " - " ++ String.fromInt standing.losses ]
        , td [ Custom.Attributes.textCentered ]
            [ text <| String.fromInt <| getTDD standing ]
        ]


viewDivision : Division -> Html Msg
viewDivision division =
    span
        ( Custom.Attributes.textButton <| ViewDivisionButtonClick division.id )
        [ text <| division.name ++ " Season " ++ String.fromInt division.season ]


viewTeamEloGraph : List EloHistory -> Html Msg
viewTeamEloGraph history =
    div []
        [ br [] []
        , h4 [] [ text "Elo History" ]
        , List.map (\h -> ( h.date, toFloat h.elo )) history
            |> LineChart.viewChart
        ]
