module Page.ListTeams exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Custom.Attributes
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Team exposing (Team, TeamId, teamsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)
import Url exposing (Protocol(..))



-- Types --


type alias Model =
    { teams : WebData (List Team)
    , navkey : Nav.Key
    , deleteError : Maybe String
    }


type Msg
    = FetchTeams
    | TeamsReceived (WebData (List Team))
    | AddTeamButtonClick
    | DeleteTeamButtonClick TeamId
    | EditTeamButtonClick TeamId
    | TeamDeleted (Result Http.Error DeleteResponse)



-- Init --


init : Nav.Key -> ( Model, Cmd Msg )
init navkey =
    ( { teams = RemoteData.Loading
      , navkey = navkey
      , deleteError = Nothing
      }
    , getTeamsRequest
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchTeams ->
            ( { model | teams = RemoteData.Loading }, getTeamsRequest )

        TeamsReceived response ->
            ( { model | teams = response }, Cmd.none )

        AddTeamButtonClick ->
            ( model, pushUrl model.navkey Route.AddTeam )

        EditTeamButtonClick id ->
            ( model, pushUrl model.navkey <| Route.EditTeam id )

        DeleteTeamButtonClick id ->
            ( model, deleteTeamRequest id )

        TeamDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getTeamsRequest )

        TeamDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. Team not found."



-- Common Helpers --


getTeamsRequest : Cmd Msg
getTeamsRequest =
    Api.getRequest Api.Teams <|
        Http.expectJson (RemoteData.fromResult >> TeamsReceived) teamsDecoder


deleteTeamRequest : TeamId -> Cmd Msg
deleteTeamRequest id =
    Api.deleteRequest (Api.Team id) <|
        Http.expectJson TeamDeleted deleteResponseDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ div Custom.Attributes.row [ viewRefreshButton ]
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
            viewTeams teams

        RemoteData.Failure httpError ->
            viewError <| Error.buildErrorMessage httpError


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div [ Custom.Attributes.errorMessage ]
        [ h3 [] [ text errorHeading ]
        , text <| "Error: " ++ errorMessage
        ]


viewTeams : List Team -> Html Msg
viewTeams teams =
    div []
        [ viewHeader
        , table [ Custom.Attributes.table ]
            [ viewTableHeader
            , tbody [] <|
                List.map viewTeam teams
            ]
        ]


viewHeader : Html Msg
viewHeader =
    div Custom.Attributes.row
        [ div [ Custom.Attributes.col ] [ h3 [] [ text "Teams" ] ]
        , div [ Custom.Attributes.col ] [ viewToolBar ]
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


viewTableHeader : Html Msg
viewTableHeader =
    thead []
        [ tr []
            [ th [ scope "col" ]
                [ text "Name" ]
            , th [ scope "col" ]
                [ text "Race" ]
            , th [ scope "col" ]
                [ text "Coach" ]
            , th [ scope "col" ]
                [ text "Elo" ]
            , th [ scope "col" ]
                [ text "" ]
            ]
        ]


viewTeam : Team -> Html Msg
viewTeam team =
    tr []
        [ td []
            [ text team.name ]
        , td []
            [ text team.race.name ]
        , td []
            [ text team.coach.name ]
        , td []
            [ text <| String.fromInt team.elo ]
        , td [ Custom.Attributes.tableButtonColumn ]
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
