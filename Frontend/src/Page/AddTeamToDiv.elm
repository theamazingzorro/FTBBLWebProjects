module Page.AddTeamToDiv exposing (Model, Msg, init, update, view)

import Api
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Division exposing (Division, DivisionId, divisionDecoder)
import Model.Session exposing (Session)
import Model.Team exposing (Team, TeamId, teamsDecoder)
import RemoteData exposing (WebData)
import Url exposing (Protocol(..))



-- Types --


type alias Model =
    { session : Session
    , divisionId : DivisionId
    , selectedTeamId : Maybe TeamId
    , saveError : Maybe String
    , division : WebData Division
    , teams : WebData (List Team)
    }


type Msg
    = TeamsReceived (WebData (List Team))
    | DivisionReceived (WebData Division)
    | TeamSelected String
    | Submit
    | TeamDivSubmitted (Result Http.Error String)



-- Init --


init : Session -> DivisionId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , divisionId = id
      , selectedTeamId = Nothing
      , saveError = Nothing
      , division = RemoteData.Loading
      , teams = RemoteData.Loading
      }
    , Cmd.batch
        [ getFreeTeamsRequest session.token
        , getDivisionRequest session.token id
        ]
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamsReceived response ->
            ( { model | teams = response }, Cmd.none )

        DivisionReceived response ->
            ( { model | division = response }, Cmd.none )

        TeamSelected idString ->
            let
                newTeamId =
                    case model.teams of
                        RemoteData.Success teams ->
                            getNewTeamId teams idString

                        _ ->
                            model.selectedTeamId
            in
            ( { model | selectedTeamId = newTeamId }, Cmd.none )

        Submit ->
            ( { model | selectedTeamId = Nothing }, trySubmit model.session.token model.selectedTeamId model.divisionId )

        TeamDivSubmitted (Ok _) ->
            ( { model | saveError = Nothing }, getFreeTeamsRequest model.session.token )

        TeamDivSubmitted (Err err) ->
            ( { model | saveError = Just (buildErrorMessage err) }, Cmd.none )


trySubmit : Maybe String -> Maybe TeamId -> DivisionId -> Cmd Msg
trySubmit token maybeTeamId divId =
    case maybeTeamId of
        Just teamId ->
            addTeamToDiv token teamId divId

        Nothing ->
            Cmd.none



-- API Requests --


getFreeTeamsRequest : Maybe String -> Cmd Msg
getFreeTeamsRequest token =
    Api.getRequest token Api.FreeTeams <|
        Http.expectJson (RemoteData.fromResult >> TeamsReceived) teamsDecoder


getDivisionRequest : Maybe String -> DivisionId -> Cmd Msg
getDivisionRequest token divId =
    Api.getRequest token (Api.Division divId) <|
        Http.expectJson (RemoteData.fromResult >> DivisionReceived) divisionDecoder


addTeamToDiv : Maybe String -> TeamId -> DivisionId -> Cmd Msg
addTeamToDiv token teamId divId =
    Api.postRequest token (Api.TeamUpdateDiv teamId divId) Http.emptyBody <|
        Http.expectString TeamDivSubmitted



-- Helpers --


getSelectedTeam : List Team -> Maybe TeamId -> Maybe Team
getSelectedTeam teams id =
    List.filter (teamHasGivenId id) teams
        |> List.head


teamHasGivenId : Maybe TeamId -> Team -> Bool
teamHasGivenId maybeId team =
    case maybeId of
        Just id ->
            team.id == id

        Nothing ->
            False


getNewTeamId : List Team -> String -> Maybe TeamId
getNewTeamId teams possibleId =
    List.filter (\team -> Model.Team.idToString team.id == possibleId) teams
        |> List.head
        |> Maybe.andThen (\team -> Just team.id)



-- View --


view : Model -> Html Msg
view model =
    case model.division of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success division ->
            row []
                [ mainHeader [] [ text <| "Add Team to " ++ division.name ]
                , viewSaveError model.saveError
                , if division.closed then
                    closedDivError

                  else
                    viewFormOrError model
                ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


closedDivError : Html Msg
closedDivError =
    errorText []
        [ emphasisText [] [ text "Invalid Division" ]
        , text "You cannot add a team to a division that has been closed."
        ]


viewFormOrError : Model -> Html Msg
viewFormOrError model =
    case model.teams of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success teams ->
            viewForm teams model

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            errorText []
                [ emphasisText [] [ text "Couldn't add team to a new division at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""


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


viewForm : List Team -> Model -> Html Msg
viewForm teams model =
    inputForm []
        [ teamDropdown model.selectedTeamId <| List.sortBy .name teams
        , viewSelectedTeam <| getSelectedTeam teams model.selectedTeamId
        ]


teamDropdown : Maybe TeamId -> List Team -> Html Msg
teamDropdown selectedId teams =
    inputSection []
        [ dropdownInput [ onInput TeamSelected ]
            (List.map (teamOption selectedId) teams)
        , inputLabel [] [ text "Team" ]
        ]


teamOption : Maybe TeamId -> Team -> ( List (Attribute msg), List (Html msg) )
teamOption selectedId team =
    ( [ value <| Model.Team.idToString team.id
      , selected <| Just team.id == selectedId
      ]
    , [ text team.name ]
    )


viewSelectedTeam : Maybe Team -> Html Msg
viewSelectedTeam maybeTeam =
    case maybeTeam of
        Just team ->
            div []
                [ viewStaticField "Race" team.race.name
                , viewStaticField "Coach" team.coach.name
                , viewStaticField "Elo" <| String.fromInt team.elo
                , addButton [ onClick Submit ] [ text "Save" ]
                ]

        Nothing ->
            text ""


viewStaticField : String -> String -> Html msg
viewStaticField lblText entry =
    Custom.Html.disabledTextInput
        [ value entry ]
        [ text lblText ]
