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
import Model.Session exposing (Session)
import Model.Team exposing (Team, TeamId, teamsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)
import Url exposing (Protocol(..))



-- Types --


type alias Model =
    { teams : WebData (List Team)
    , sortingMethod : SortingMethod
    , division : WebData Division
    , divisionId : DivisionId
    , session : Session
    , deleteError : Maybe String
    }


type Msg
    = RefreshButtonClick
    | TeamsReceived (WebData (List Team))
    | DivisionReceived (WebData Division)
    | AddTeamButtonClick
    | DeleteTeamButtonClick TeamId
    | EditTeamButtonClick TeamId
    | TeamDeleted (Result Http.Error DeleteResponse)
    | NameSortClick
    | RaceSortClick
    | CoachSortClick
    | EloSortClick


type SortingMethod
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
      , session = session
      , deleteError = Nothing
      , divisionId = id
      }
    , Cmd.batch
        [ getTeamsInDivRequest session.token id
        , getDivisionRequest session.token id
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
                ]
            )

        TeamsReceived response ->
            ( { model | teams = response }, Cmd.none )

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

        NameSortClick ->
            ( { model | sortingMethod = newSort Name NameDesc model.sortingMethod }, Cmd.none )

        RaceSortClick ->
            ( { model | sortingMethod = newSort Race RaceDesc model.sortingMethod }, Cmd.none )

        CoachSortClick ->
            ( { model | sortingMethod = newSort Coach CoachDesc model.sortingMethod }, Cmd.none )

        EloSortClick ->
            ( { model | sortingMethod = newSort Elo EloDesc model.sortingMethod }, Cmd.none )


newSort : SortingMethod -> SortingMethod -> SortingMethod -> SortingMethod
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


getDivisionRequest : Maybe String -> DivisionId -> Cmd Msg
getDivisionRequest token divId =
    Api.getRequest token (Api.Division divId) <|
        Http.expectJson (RemoteData.fromResult >> DivisionReceived) divisionDecoder


deleteTeamRequest : Maybe String -> TeamId -> Cmd Msg
deleteTeamRequest token id =
    Api.deleteRequest token (Api.Team id) <|
        Http.expectJson TeamDeleted deleteResponseDecoder



-- Helper Functions --


sortedTeams : SortingMethod -> List Team -> List Team
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


viewTeamsOrError : Model -> Html Msg
viewTeamsOrError model =
    case model.teams of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success teams ->
            viewTeams model teams

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


viewTeams : Model -> List Team -> Html Msg
viewTeams model teams =
    div []
        [ viewHeaderOrError model.division model.session
        , table [ Custom.Attributes.table ]
            [ viewTableHeader model.sortingMethod
            , tbody [] <|
                List.map (viewTeam model.session) <|
                    sortedTeams model.sortingMethod teams
            ]
        ]


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


viewTableHeader : SortingMethod -> Html Msg
viewTableHeader sortMethod =
    thead []
        [ tr []
            [ th [ scope "col", onClick NameSortClick ]
                [ case sortMethod of
                    Name ->
                        text "Name ▲"

                    NameDesc ->
                        text "Name ▼"

                    _ ->
                        text "Name"
                ]
            , th [ scope "col", onClick RaceSortClick ]
                [ case sortMethod of
                    Race ->
                        text "Race ▲"

                    RaceDesc ->
                        text "Race ▼"

                    _ ->
                        text "Race"
                ]
            , th [ scope "col", onClick CoachSortClick ]
                [ case sortMethod of
                    Coach ->
                        text "Coach ▲"

                    CoachDesc ->
                        text "Coach ▼"

                    _ ->
                        text "Coach"
                ]
            , th [ scope "col", onClick EloSortClick ]
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


viewTeam : Session -> Team -> Html Msg
viewTeam session team =
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
