module Page.AddTeamToDiv exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Attributes
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Division exposing (Division, DivisionId, divisionDecoder)
import Model.Session exposing (Session)
import Model.Team exposing (Team, TeamId, teamsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)
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
        [ getTeamsNotInDivRequest session.token id
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
            ( { model | selectedTeamId = Nothing },  trySubmit model.session.token model.selectedTeamId model.divisionId)

        TeamDivSubmitted (Ok _) ->
            ( { model | saveError = Nothing }, pushUrl model.session.navkey <| Route.ViewDivision model.divisionId )

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


getTeamsNotInDivRequest : Maybe String -> DivisionId -> Cmd Msg
getTeamsNotInDivRequest token divId =
    Api.getRequest token (Api.TeamsNotInDiv divId) <|
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
            h3 [] [ text "Loading..." ]

        RemoteData.Success division ->
            div []
                [ h3 [] [ text <| "Add Team to " ++ division.name ]
                , br [] []
                , viewSaveError model.saveError
                , viewFormOrError model
                ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewFormOrError : Model -> Html Msg
viewFormOrError model =
    case model.teams of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success teams ->
            viewForm teams model

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't add team to a new division at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


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


viewForm : List Team -> Model -> Html Msg
viewForm teams model =
    div []
        [ teamDropdown model.selectedTeamId teams
        , viewSelectedTeam <| getSelectedTeam teams model.selectedTeamId
        ]


teamDropdown : Maybe TeamId -> List Team -> Html Msg
teamDropdown selectedId teams =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "teamDropdown")
            [ text "Team" ]
        , select
            (Custom.Attributes.formDropdown "teamDropdown"
                [ onInput TeamSelected ]
            )
            (defaultOption :: List.map (teamOption selectedId) teams)
        ]


defaultOption : Html Msg
defaultOption =
    option [ value "0" ] [ text "-" ]


teamOption : Maybe TeamId -> Team -> Html msg
teamOption selectedId team =
    option
        [ value <| Model.Team.idToString team.id
        , selected (Just team.id == selectedId)
        ]
        [ text team.name ]


viewSelectedTeam : Maybe Team -> Html Msg
viewSelectedTeam maybeTeam =
    case maybeTeam of
        Just team ->
            div []
                [ viewStaticField "raceField" "Race" team.race.name
                , viewStaticField "coachField" "Coach" team.coach.name
                , viewStaticField "eloField" "Elo" <| String.fromInt team.elo
                , button
                    [ Custom.Attributes.submitButton
                    , onClick Submit
                    ]
                    [ text "Save" ]
                ]

        Nothing ->
            text ""


viewStaticField : String -> String -> String -> Html msg
viewStaticField id lblText entry =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel id)
            [ text lblText ]
        , input
            (Custom.Attributes.formInput id
                [ readonly True
                , value entry
                ]
            )
            []
        ]
