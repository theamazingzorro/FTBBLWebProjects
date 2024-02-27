module Page.ListTeams exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, div, span, text)
import Html.Events exposing (onClick)
import Http
import List exposing (drop, length, take)
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
    , page : Int
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
    | FirstPageClick
    | PrevPageClick
    | NextPageClick
    | LastPageClick


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
      , page = 0
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
            ( { model | sortingMethod = newSort DivisionDesc Division model.sortingMethod }, Cmd.none )

        FirstPageClick ->
            ( { model | page = 0 }, Cmd.none )

        PrevPageClick ->
            ( { model | page = Basics.max 0 (model.page - 1) }, Cmd.none )

        NextPageClick ->
            ( { model | page = Basics.min (lastPage model.teams) (model.page + 1) }, Cmd.none )

        LastPageClick ->
            ( { model | page = lastPage model.teams }, Cmd.none )


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
    let
        compareStrIgnoreCase a b =
            compare (toLower a) (toLower b)

        compareCoachName a b =
            compareStrIgnoreCase a.coach.name b.coach.name

        compareRace a b =
            compareStrIgnoreCase a.race.name b.race.name

        secondarySortElo primarySort a b =
            case primarySort a b of
                EQ ->
                    compare b.elo a.elo

                other ->
                    other

        compareElo a b =
            compare a.elo b.elo

        compareName a b =
            compareStrIgnoreCase a.name b.name

        reverse func a b =
            func b a

        compareMaybeDiv comparison a b =
            case a.division of
                Just aDiv ->
                    case b.division of
                        Just bDiv ->
                            comparison aDiv bDiv

                        Nothing ->
                            GT

                Nothing ->
                    case b.division of
                        Just _ ->
                            LT

                        Nothing ->
                            EQ
    in
    case sortingMethod of
        Default ->
            List.sortWith (reverse compareElo) teams

        Name ->
            List.sortWith compareName teams

        NameDesc ->
            List.sortWith (reverse compareName) teams

        Coach ->
            List.sortWith (secondarySortElo compareCoachName) teams

        CoachDesc ->
            List.sortWith (secondarySortElo <| reverse compareCoachName) teams

        Race ->
            List.sortWith (secondarySortElo compareRace) teams

        RaceDesc ->
            List.sortWith (secondarySortElo <| reverse compareRace) teams

        Elo ->
            List.sortWith compareElo teams

        EloDesc ->
            List.sortWith (reverse compareElo) teams

        Division ->
            List.sortWith (secondarySortElo <| compareMaybeDiv compareDivisions) teams

        DivisionDesc ->
            List.sortWith (secondarySortElo <| reverse <| compareMaybeDiv compareDivisions) teams


pageSize : Int
pageSize =
    20


pageOfList : Int -> List a -> List a
pageOfList page list =
    list
        |> drop (pageSize * page)
        |> take pageSize


lastPage : WebData (List a) -> Int
lastPage list =
    case list of
        RemoteData.Success l ->
            length l // pageSize

        _ ->
            0



-- View --


view : Model -> Html Msg
view model =
    row []
        [ viewRefreshButton
        , viewErrorMessage model.deleteError
        , viewTeamsOrError model
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    optionButton [ onClick FetchTeams, floatRight ] [ text "Refresh Teams" ]


viewTeamsOrError : Model -> Html Msg
viewTeamsOrError model =
    case model.teams of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success teams ->
            viewTeams model.session model.sortingMethod model.page teams

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
            errorText []
                [ text <| "Error: " ++ m ]

        Nothing ->
            text ""


viewTeams : Session -> SortingMethod -> Int -> List Team -> Html Msg
viewTeams session sortMethod page teams =
    div []
        [ viewHeader session
        , table []
            [ viewTableHeader session sortMethod
            , tableBody []
                (sortedTeams sortMethod teams
                    |> pageOfList page
                    |> List.map (viewTeam session)
                )
            ]
        , viewPageSelect page (length teams)
        ]


viewHeader : Session -> Html Msg
viewHeader session =
    row []
        [ mainHeader [] [ text "Teams" ]
        , requiresAuth session viewAddButton
        ]


viewAddButton : Html Msg
viewAddButton =
    addButton
        [ onClick AddTeamButtonClick, floatRight ]
        [ text "Add Team" ]


viewTableHeader : Session -> SortingMethod -> Html Msg
viewTableHeader session sortMethod =
    tableHead []
        [ ( [ onClick NameSortClick ]
          , [ case sortMethod of
                Name ->
                    text "Name ▲"

                NameDesc ->
                    text "Name ▼"

                _ ->
                    text "Name"
            ]
          )
        , ( [ onClick RaceSortClick ]
          , [ case sortMethod of
                Race ->
                    text "Race ▲"

                RaceDesc ->
                    text "Race ▼"

                _ ->
                    text "Race"
            ]
          )
        , ( [ onClick CoachSortClick ]
          , [ case sortMethod of
                Coach ->
                    text "Coach ▲"

                CoachDesc ->
                    text "Coach ▼"

                _ ->
                    text "Coach"
            ]
          )
        , ( [ onClick DivisionSortClick ]
          , [ case sortMethod of
                Division ->
                    text "Division ▲"

                DivisionDesc ->
                    text "Division ▼"

                _ ->
                    text "Division"
            ]
          )
        , ( [ onClick EloSortClick ]
          , [ case sortMethod of
                Elo ->
                    text "Elo ▲"

                EloDesc ->
                    text "Elo ▼"

                _ ->
                    text "Elo"
            ]
          )
        , ( [], [ requiresAuth session <| text " " ] )

        {- TODO: likely will show something either way NEED FIX -}
        ]


viewTeam : Session -> Team -> Html Msg
viewTeam session team =
    tableRow []
        [ ( []
          , [ pageLink
                [ onClick <| ViewTeamClick team.id ]
                [ text team.name ]
            , viewAccolades team.accolades
            ]
          )
        , ( [], [ text team.race.name ] )
        , ( []
          , [ pageLink
                [ onClick <| ViewCoachClick team.coach.id ]
                [ text team.coach.name ]
            , viewAccolades team.coach.accolades
            ]
          )
        , ( [], [ viewDivision team ] )
        , ( []
          , [ text <| String.fromInt team.elo ]
          )
        , ( []
          , [ requiresAuth session <| viewEditButton team, requiresAuth session <| viewDeleteButton team ]
          )
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
            pageLink
                [ onClick <| ViewDivisionButtonClick division.id ]
                [ text <| division.name ++ " Season " ++ String.fromInt division.season ]

        Nothing ->
            text ""


viewDeleteButton : Team -> Html Msg
viewDeleteButton team =
    warnButton
        [ onClick (DeleteTeamButtonClick team.id) ]
        [ text "Delete" ]


viewEditButton : Team -> Html Msg
viewEditButton team =
    optionButton
        [ onClick (EditTeamButtonClick team.id) ]
        [ text "Edit" ]


viewPageSelect : Int -> Int -> Html Msg
viewPageSelect page teamsCount =
    if teamsCount <= pageSize then
        text ""

    else
        pageBar []
            [ pageBarButton [ onClick FirstPageClick ] [ text "<<" ]
            , pageBarButton [ onClick PrevPageClick ] [ text "<" ]
            , pageBarFiller [] [ text <| String.fromInt (page + 1) ++ " of " ++ String.fromInt (teamsCount // pageSize + 1) ]
            , pageBarButton [ onClick NextPageClick ] [ text ">" ]
            , pageBarButton [ onClick LastPageClick ] [ text ">>" ]
            ]
