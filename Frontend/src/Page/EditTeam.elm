module Page.EditTeam exposing (Model, Msg, init, update, view)

import Api exposing (Endpoint(..))
import Custom.Events exposing (onEnter)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Coach exposing (Coach, CoachId)
import Model.Session exposing (Session)
import Model.Team exposing (Team, TeamId, teamDecoder, teamEncoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { session : Session
    , id : TeamId
    , team : WebData Team
    , saveError : Maybe String
    }


type Msg
    = TeamReceived (WebData Team)
    | NameChanged String
    | CoachLinkClicked CoachId
    | Submit
    | TeamSubmitted (Result Http.Error Team)



-- Init --


init : Session -> TeamId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , team = RemoteData.Loading
      , saveError = Nothing
      }
    , getTeamRequest session.token id
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamReceived response ->
            ( { model | team = response }, Cmd.none )

        NameChanged newName ->
            ( { model | team = rename model.team newName }, Cmd.none )

        CoachLinkClicked id ->
            ( model, pushUrl model.session.navkey <| Route.EditCoach id )

        Submit ->
            trySaveTeam model

        TeamSubmitted (Ok _) ->
            ( { model | saveError = Nothing }, pushUrl model.session.navkey Route.Teams )

        TeamSubmitted (Err err) ->
            ( { model | saveError = Just (buildErrorMessage err) }, Cmd.none )


rename : WebData Team -> String -> WebData Team
rename team newName =
    case team of
        RemoteData.Success oldTeam ->
            RemoteData.Success { oldTeam | name = newName }

        _ ->
            team


trySaveTeam : Model -> ( Model, Cmd Msg )
trySaveTeam model =
    case model.team of
        RemoteData.Success team ->
            ( { model | saveError = Nothing }, saveTeam model.session.token team )

        _ ->
            ( { model | saveError = Just "Cannot submit data, please refresh page and try again." }, Cmd.none )



-- API Requests --


getTeamRequest : Maybe String -> TeamId -> Cmd Msg
getTeamRequest token id =
    Api.getRequest token (Api.Team id) <|
        Http.expectJson (RemoteData.fromResult >> TeamReceived) teamDecoder


saveTeam : Maybe String -> Team -> Cmd Msg
saveTeam token team =
    Api.putRequest token
        (Api.Team team.id)
        (Http.jsonBody (teamEncoder team))
    <|
        Http.expectJson TeamSubmitted teamDecoder



-- View --


view : Model -> Html Msg
view model =
    row []
        [ mainHeader [] [ text "Edit Team" ]
        , viewSaveError model.saveError
        , viewTeamOrError model
        ]


viewTeamOrError : Model -> Html Msg
viewTeamOrError model =
    case model.team of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success team ->
            viewTeam team

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            errorText []
                [ emphasisText [] [ text "Couldn't save a team at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""


viewLoadError : String -> Html Msg
viewLoadError errorMessage =
    errorText []
        [ emphasisText [] [ text "Couldn't fetch data at this time." ]
        , text <| "Error: " ++ errorMessage
        ]


viewTeam : Team -> Html Msg
viewTeam team =
    inputForm []
        [ viewNameField team.name
        , disabledTextInput [ value team.race.name ] [ text "Race" ]
        , viewCoachField team.coach
        , disabledTextInput [ value <| String.fromInt team.elo ] [ text "Elo" ]
        , submitButton Submit [ text "Save" ]
        ]


viewNameField : String -> Html Msg
viewNameField name =
    textInput
        [ onInput NameChanged
        , onEnter Submit
        , value name
        ]
        [ text "Name" ]


viewCoachField : Coach -> Html Msg
viewCoachField coach =
    disabledTextInput
        [ value coach.name
        , onClick <| CoachLinkClicked coach.id
        , fakeLink
        ]
        [ text "Coach" ]
