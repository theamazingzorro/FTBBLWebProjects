module Page.ViewCoach exposing (Model, Msg, init, update, view)

import Api exposing (Endpoint(..))
import Custom.Attributes
import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import LineChart
import Model.Accolade exposing (Accolade, viewAccolade)
import Model.Coach exposing (Coach, coachDecoder)
import Model.Division exposing (Division, DivisionId)
import Model.EloHistory exposing (EloHistory, historyListDecoder, maxElo)
import Model.Session exposing (Session)
import Model.SharedIds exposing (CoachId)
import Model.Team exposing (Team, TeamId, teamsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { session : Session
    , id : CoachId
    , coach : WebData Coach
    , teams : WebData (List Team)
    , coachHistory : WebData (List EloHistory)
    }


type Msg
    = CoachReceived (WebData Coach)
    | HistoryReceived (WebData (List EloHistory))
    | TeamsReceived (WebData (List Team))
    | ViewTeamClick TeamId
    | ViewDivisionClick DivisionId



-- Init --


init : Session -> CoachId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , coach = RemoteData.Loading
      , teams = RemoteData.Loading
      , coachHistory = RemoteData.Loading
      }
    , Cmd.batch
        [ getTeamsRequest session.token id
        , getTeamHistoryRequest session.token id
        , getCoachRequest session.token id
        ]
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamsReceived response ->
            ( { model | teams = response }, Cmd.none )

        HistoryReceived response ->
            ( { model | coachHistory = response }, Cmd.none )

        CoachReceived response ->
            ( { model | coach = response }, Cmd.none )

        ViewTeamClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewTeam id )

        ViewDivisionClick divId ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision divId )



-- API Requests --


getTeamsRequest : Maybe String -> CoachId -> Cmd Msg
getTeamsRequest token id =
    Api.getRequest token (Api.TeamsWithCoach id) <|
        Http.expectJson (RemoteData.fromResult >> TeamsReceived) teamsDecoder


getTeamHistoryRequest : Maybe String -> CoachId -> Cmd Msg
getTeamHistoryRequest token id =
    Api.getRequest token (Api.CoachEloHistory id) <|
        Http.expectJson (RemoteData.fromResult >> HistoryReceived) historyListDecoder


getCoachRequest : Maybe String -> CoachId -> Cmd Msg
getCoachRequest token id =
    Api.getRequest token (Api.Coach id) <|
        Http.expectJson (RemoteData.fromResult >> CoachReceived) coachDecoder



-- View --


view : Model -> Html Msg
view model =
    case model.coach of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success coach ->
            viewCoach model coach

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


viewCoach : Model -> Coach -> Html Msg
viewCoach model coach =
    div []
        [ br [] []
        , viewCoachDetails model coach
        , viewTeams model
        , viewCoachEloHistory model
        ]


viewTeams : Model -> Html Msg
viewTeams model =
    case model.teams of
        RemoteData.Success teams ->
            viewTeamsTable teams

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Teams..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewCoachEloHistory : Model -> Html Msg
viewCoachEloHistory model =
    case model.coachHistory of
        RemoteData.Success history ->
            viewCoachEloGraph history

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading Elo History..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewCoachDetails : Model -> Coach -> Html Msg
viewCoachDetails model coach =
    div [ class "row" ]
        [ div [ class " col" ]
            [ h3 [] [ text coach.name ]
            , br [] []
            , p [] [ text <| "Current Elo: " ++ String.fromInt coach.elo ]
            , p [] [ text <| "Max Elo: " ++ viewMaxElo model.coachHistory ]
            ]
        , div [ class "col" ]
            [ if coach.accolades /= [] then
                viewAccolades coach

              else
                text ""
            ]
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


viewAccolades : Coach -> Html Msg
viewAccolades coach =
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
            List.map viewAccoladeRow coach.accolades
        ]


viewAccoladeRow : Accolade -> Html Msg
viewAccoladeRow accolade =
    tr []
        [ td []
            [ viewAccolade accolade ]
        , td []
            [ text <| accolade.name ++ (Maybe.map (\season -> " Season " ++ String.fromInt season) accolade.season |> Maybe.withDefault "") ]
        ]


viewTeamsTable : List Team -> Html Msg
viewTeamsTable teams =
    div []
        [ br [] []
        , br [] []
        , h4 [] [ text "Teams" ]
        , table [ Custom.Attributes.table ]
            [ viewTableHeader
            , tbody [] <|
                List.map viewTableRow teams
            ]
        ]


viewTableHeader : Html Msg
viewTableHeader =
    thead []
        [ tr []
            [ th [ scope "col" ]
                [ text "Name" ]
            , th [ scope "col" ]
                [ text "Race" ]
            , th [ scope "col" ]
                [ text "Division" ]
            , th [ scope "col", Custom.Attributes.textCentered ]
                [ text "Elo" ]
            ]
        ]


viewTableRow : Team -> Html Msg
viewTableRow team =
    tr []
        [ td []
            [ span
                (Custom.Attributes.textButton <| ViewTeamClick team.id)
                [ text team.name ]
            , viewSmallAccolades team.accolades
            ]
        , td []
            [ text team.race.name ]
        , td []
            [ viewDivision team.division ]
        , td [ Custom.Attributes.textCentered ]
            [ text <| String.fromInt team.elo ]
        ]


viewSmallAccolades : List Accolade -> Html Msg
viewSmallAccolades accolades =
    span []
        (List.sortWith (\a b -> compare (Maybe.withDefault 0 b.season) (Maybe.withDefault 0 a.season)) accolades
            |> List.take 3
            |> List.map viewAccolade
        )


viewDivision : Maybe Division -> Html Msg
viewDivision maybeDiv =
    case maybeDiv of
        Nothing ->
            text ""

        Just division ->
            span
                (Custom.Attributes.textButton <| ViewDivisionClick division.id)
                [ text <| division.name ++ " Season " ++ String.fromInt division.season ]


viewCoachEloGraph : List EloHistory -> Html Msg
viewCoachEloGraph history =
    div []
        [ br [] []
        , h4 [] [ text "Elo History" ]
        , List.map (\h -> ( h.date, toFloat h.elo )) history
            |> LineChart.viewChart
        ]
