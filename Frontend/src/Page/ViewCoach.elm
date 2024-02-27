module Page.ViewCoach exposing (Model, Msg, init, update, view)

import Api exposing (Endpoint(..))
import Auth exposing (requiresAuth)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, div, span, text)
import Html.Events exposing (onClick)
import Http
import LineChart
import Model.Accolade exposing (Accolade, viewAccolade)
import Model.Coach exposing (Coach, coachDecoder)
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Division exposing (Division, DivisionId, compareDivisions)
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
    , deleteError : Maybe String
    }


type Msg
    = CoachReceived (WebData Coach)
    | HistoryReceived (WebData (List EloHistory))
    | TeamsReceived (WebData (List Team))
    | ViewTeamClick TeamId
    | ViewDivisionClick DivisionId
    | DeleteCoachButtonClick
    | EditCoachButtonClick
    | AddAccoladeButtonClick
    | CoachDeleted (Result Http.Error DeleteResponse)



-- Init --


init : Session -> CoachId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , coach = RemoteData.Loading
      , teams = RemoteData.Loading
      , coachHistory = RemoteData.Loading
      , deleteError = Nothing
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
            ( { model | teams = sortTeamsChron response }, Cmd.none )

        HistoryReceived response ->
            ( { model | coachHistory = response }, Cmd.none )

        CoachReceived response ->
            ( { model | coach = response }, Cmd.none )

        ViewTeamClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewTeam id )

        ViewDivisionClick divId ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision divId )

        AddAccoladeButtonClick ->
            ( model, pushUrl model.session.navkey <| Route.AddAccoladeWithDefaults Nothing model.id )

        EditCoachButtonClick ->
            ( model, pushUrl model.session.navkey <| Route.EditCoach model.id )

        DeleteCoachButtonClick ->
            ( model, deleteCoachRequest model.session.token model.id )

        CoachDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }
            , if res.deleted then
                pushUrl model.session.navkey Route.Coaches

              else
                Cmd.none
            )

        CoachDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. Coach not found."


sortTeamsChron : WebData (List Team) -> WebData (List Team)
sortTeamsChron data =
    let
        compareMaybeDiv a b =
            case a.division of
                Just aDiv ->
                    case b.division of
                        Just bDiv ->
                            compareDivisions aDiv bDiv

                        Nothing ->
                            GT

                Nothing ->
                    case b.division of
                        Just _ ->
                            LT

                        Nothing ->
                            EQ
    in
    case data of
        RemoteData.Success teams ->
            RemoteData.Success <| List.reverse <| List.sortWith compareMaybeDiv teams

        other ->
            other



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


deleteCoachRequest : Maybe String -> CoachId -> Cmd Msg
deleteCoachRequest token id =
    Api.deleteRequest token (Api.Coach id) <|
        Http.expectJson CoachDeleted deleteResponseDecoder



-- View --


view : Model -> Html Msg
view model =
    case model.coach of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success coach ->
            row []
                [ requiresAuth model.session viewToolBar
                , viewErrorMessage model.deleteError
                , viewCoach model coach
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


viewToolBar : Html Msg
viewToolBar =
    row [ floatRight ]
        [ addButton
            [ onClick AddAccoladeButtonClick ]
            [ text "Add Accolade" ]
        , optionButton
            [ onClick EditCoachButtonClick ]
            [ text "Edit" ]
        , warnButton
            [ onClick DeleteCoachButtonClick ]
            [ text "Delete" ]
        ]


viewCoach : Model -> Coach -> Html Msg
viewCoach model coach =
    div []
        [ viewCoachDetails model coach
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
            emphasisText [] [ text "Loading Teams..." ]

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
            emphasisText [] [ text "Loading Elo History..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewCoachDetails : Model -> Coach -> Html Msg
viewCoachDetails model coach =
    row []
        [ colTwoThird []
            [ mainHeader [] [ text coach.name ]
            , bodyText [] [ text <| "Current Elo: " ++ String.fromInt coach.elo ]
            , bodyText [] [ text <| "Max Elo: " ++ viewMaxElo model.coachHistory ]
            ]
        , colThird []
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
    table []
        [ tableHead []
            [ ( [], [ text "" ] )
            , ( [], [ text "Achievements" ] )
            ]
        , tableBody [] <|
            List.map viewAccoladeRow coach.accolades
        ]


viewAccoladeRow : Accolade -> Html Msg
viewAccoladeRow accolade =
    tableRow []
        [ ( [], [ viewAccolade accolade ] )
        , ( [], [ text <| accolade.name ++ (Maybe.map (\season -> " Season " ++ String.fromInt season) accolade.season |> Maybe.withDefault "") ] )
        ]


viewTeamsTable : List Team -> Html Msg
viewTeamsTable teams =
    row []
        [ subHeader [] [ text "Teams" ]
        , table []
            [ viewTeamTableHeader
            , tableBody [] <|
                List.map viewTableRow teams
            ]
        ]


viewTeamTableHeader : Html Msg
viewTeamTableHeader =
    tableHead []
        [ ( [], [ text "Name" ] )
        , ( [], [ text "Race" ] )
        , ( [], [ text "Division" ] )
        , ( [], [ text "Elo" ] )
        ]


viewTableRow : Team -> Html Msg
viewTableRow team =
    tableRow []
        [ ( []
          , [ pageLink
                [ onClick <| ViewTeamClick team.id ]
                [ text team.name ]
            , viewSmallAccolades team.accolades
            ]
          )
        , ( [], [ text team.race.name ] )
        , ( [], [ viewDivision team.division ] )
        , ( [], [ text <| String.fromInt team.elo ] )
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
            pageLink
                [ onClick <| ViewDivisionClick division.id ]
                [ text <| division.name ++ " Season " ++ String.fromInt division.season ]


viewCoachEloGraph : List EloHistory -> Html Msg
viewCoachEloGraph history =
    row []
        [ subHeader [] [ text "Elo History" ]
        , List.map (\h -> ( h.date, toFloat h.elo )) history
            |> LineChart.viewChart
        ]
