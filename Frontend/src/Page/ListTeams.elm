module Page.ListTeams exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Attributes exposing (textButton, textCentered)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.Accolade exposing (Accolade, viewAccolade)
import Model.Coach exposing (Coach, CoachId)
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Division exposing (Division, DivisionId, compareDivisions)
import Model.Session exposing (Session)
import Model.Team exposing (Team, TeamId, teamsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)
import String exposing (toLower)
import Url exposing (Protocol(..))



-- Types --


type alias Model =
    { teams : WebData (List Team)
    , sortingMethod : SortingMethod
    , session : Session
    , deleteError : Maybe String
    }


type Msg
    = FetchTeams
    | TeamsReceived (WebData (List Team))
    | AddTeamButtonClick
    | DeleteTeamButtonClick TeamId
    | EditTeamButtonClick TeamId
    | ViewDivisionButtonClick DivisionId
    | ViewTeamClick TeamId
    | ViewCoachClick CoachId
    | TeamDeleted (Result Http.Error DeleteResponse)
    | NameSortClick
    | RaceSortClick
    | CoachSortClick
    | EloSortClick
    | DivisionSortClick


type SortingMethod
    = Default
    | Name
    | NameDesc
    | Race
    | RaceDesc
    | Coach
    | CoachDesc
    | Elo
    | EloDesc
    | Division
    | DivisionDesc



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { teams = RemoteData.Loading
      , sortingMethod = Default
      , session = session
      , deleteError = Nothing
      }
    , getTeamsRequest session.token
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchTeams ->
            ( { model | teams = RemoteData.Loading }, getTeamsRequest model.session.token )

        TeamsReceived response ->
            ( { model | teams = response }, Cmd.none )

        AddTeamButtonClick ->
            ( model, pushUrl model.session.navkey Route.AddTeam )

        EditTeamButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.EditTeam id )

        DeleteTeamButtonClick id ->
            ( model, deleteTeamRequest model.session.token id )

        ViewTeamClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewTeam id )

        ViewCoachClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewCoach id )

        TeamDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getTeamsRequest model.session.token )

        TeamDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )

        ViewDivisionButtonClick divId ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision divId )

        NameSortClick ->
            ( { model | sortingMethod = newSort Name NameDesc model.sortingMethod }, Cmd.none )

        RaceSortClick ->
            ( { model | sortingMethod = newSort Race RaceDesc model.sortingMethod }, Cmd.none )

        CoachSortClick ->
            ( { model | sortingMethod = newSort Coach CoachDesc model.sortingMethod }, Cmd.none )

        EloSortClick ->
            ( { model | sortingMethod = newSort EloDesc Elo model.sortingMethod }, Cmd.none )

        DivisionSortClick ->
            ( { model | sortingMethod = newSort Division DivisionDesc model.sortingMethod }, Cmd.none )


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


getTeamsRequest : Maybe String -> Cmd Msg
getTeamsRequest token =
    Api.getRequest token Api.Teams <|
        Http.expectJson (RemoteData.fromResult >> TeamsReceived) teamsDecoder


deleteTeamRequest : Maybe String -> TeamId -> Cmd Msg
deleteTeamRequest token id =
    Api.deleteRequest token (Api.Team id) <|
        Http.expectJson TeamDeleted deleteResponseDecoder



-- Helper Functions --


sortedTeams : SortingMethod -> List Team -> List Team
sortedTeams sortingMethod teams =
    case sortingMethod of
        Default ->
            List.sortWith (\a b -> compare b.elo a.elo) teams

        Name ->
            List.sortWith (\a b -> compareStrIgnoreCase a.name b.name) teams

        NameDesc ->
            List.sortWith (\a b -> compareStrIgnoreCase b.name a.name) teams

        Coach ->
            List.sortWith (\a b -> compareStrIgnoreCase a.coach.name b.coach.name) teams

        CoachDesc ->
            List.sortWith (\a b -> compareStrIgnoreCase b.coach.name a.coach.name) teams

        Race ->
            List.sortWith (\a b -> compareStrIgnoreCase a.race.name b.race.name) teams

        RaceDesc ->
            List.sortWith (\a b -> compareStrIgnoreCase b.race.name a.race.name) teams

        Elo ->
            List.sortWith (\a b -> compare a.elo b.elo) teams

        EloDesc ->
            List.sortWith (\a b -> compare b.elo a.elo) teams

        Division ->
            List.sortWith (compareMaybeDiv compareDivisions) teams

        DivisionDesc ->
            List.sortWith (compareMaybeDiv (\x y -> compareDivisions y x)) teams


compareStrIgnoreCase : String -> String -> Order
compareStrIgnoreCase a b =
    compare (toLower a) (toLower b)


compareMaybeDiv : (Division -> Division -> Order) -> Team -> Team -> Order
compareMaybeDiv comparison a b =
    case a.division of
        Just aDiv ->
            case b.division of
                Just bDiv ->
                    comparison aDiv bDiv

                Nothing ->
                    LT

        Nothing ->
            case b.division of
                Just _ ->
                    GT

                Nothing ->
                    EQ



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
            [ onClick FetchTeams
            , Custom.Attributes.refreshButton
            ]
            [ text "Refresh Teams" ]
        ]


viewTeamsOrError : Model -> Html Msg
viewTeamsOrError model =
    case model.teams of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success teams ->
            viewTeams model.session model.sortingMethod teams

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


viewTeams : Session -> SortingMethod -> List Team -> Html Msg
viewTeams session sortMethod teams =
    div []
        [ viewHeader session
        , table [ Custom.Attributes.table ]
            [ viewTableHeader sortMethod
            , tbody [] <|
                List.map (viewTeam session) <|
                    sortedTeams sortMethod teams
            ]
        ]


viewHeader : Session -> Html Msg
viewHeader session =
    div Custom.Attributes.row
        [ div [ Custom.Attributes.col ] [ h3 [] [ text "Teams" ] ]
        , div [ Custom.Attributes.col ] [ requiresAuth session viewToolBar ]
        ]


viewToolBar : Html Msg
viewToolBar =
    div [ Custom.Attributes.rightSideButtons ]
        [ button
            [ Custom.Attributes.addButton
            , onClick AddTeamButtonClick
            ]
            [ text "Add Team" ]
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
            , th [ scope "col", onClick DivisionSortClick ]
                [ case sortMethod of
                    Division ->
                        text "Division ▲"

                    DivisionDesc ->
                        text "Division ▼"

                    _ ->
                        text "Division"
                ]
            , th [ scope "col", onClick EloSortClick, textCentered ]
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
            [ span
                (textButton <| ViewTeamClick team.id)
                [ text team.name ]
            , viewAccolades team.accolades
            ]
        , td []
            [ text team.race.name ]
        , td []
            [ span
                (textButton <| ViewCoachClick team.coach.id)
                [ text team.coach.name ]
            , viewAccolades team.coach.accolades
            ]
        , td []
            [ viewDivision team ]
        , td [ textCentered ]
            [ text <| String.fromInt team.elo ]
        , requiresAuth session <|
            td (Custom.Attributes.tableButtonColumn 2)
                [ viewEditButton team, viewDeleteButton team ]
        ]


viewAccolades : List Accolade -> Html Msg
viewAccolades accolades =
    span []
        (List.sortWith (\a b -> compare (Maybe.withDefault 0 b.season) (Maybe.withDefault 0 a.season)) accolades
            |> List.take 3
            |> List.map viewAccolade
        )


viewDivision : Team -> Html Msg
viewDivision team =
    case team.division of
        Just division ->
            span
                (Custom.Attributes.textButton <| ViewDivisionButtonClick division.id)
                [ text <| division.name ++ " Season " ++ String.fromInt division.season ]

        Nothing ->
            text ""


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
