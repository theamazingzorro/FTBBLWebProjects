module Page.ViewTeam exposing (Model, Msg, init, update, view)

import Api exposing (Endpoint(..))
import Custom.Attributes
import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import LineChart
import Model.EloHistory exposing (EloHistory, historyListDecoder)
import Model.Session exposing (Session)
import Model.Team exposing (Team, TeamId, teamDecoder)
import RemoteData exposing (WebData)



-- Model --


type alias Model =
    { session : Session
    , id : TeamId
    , team : WebData Team
    , teamHistory : WebData (List EloHistory)
    }


type Msg
    = TeamReceived (WebData Team)
    | HistoryReceived (WebData (List EloHistory))



-- Init --


init : Session -> TeamId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , team = RemoteData.Loading
      , teamHistory = RemoteData.Loading
      }
    , Cmd.batch
        [ getTeamRequest session.token id
        , getTeamHistoryRequest session.token id
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



-- API Requests --


getTeamRequest : Maybe String -> TeamId -> Cmd Msg
getTeamRequest token id =
    Api.getRequest token (Api.Team id) <|
        Http.expectJson (RemoteData.fromResult >> TeamReceived) teamDecoder


getTeamHistoryRequest : Maybe String -> TeamId -> Cmd Msg
getTeamHistoryRequest token id =
    Api.getRequest token (Api.TeamEloHistory id) <|
        Http.expectJson (RemoteData.fromResult >> HistoryReceived) historyListDecoder



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
        , h3 [] [ text team.name ]
        , br [] []
        , viewTeamDetails team
        , viewTeamHistory model
        ]


viewTeamDetails : Team -> Html Msg
viewTeamDetails team =
    div []
        [ p [] [ text <| "Current Elo : " ++ String.fromInt team.elo ]
        , p [] [ text <| "Recent Division : " ++ (Maybe.map .name team.division |> Maybe.withDefault "") ]
        ]


viewTeamHistory : Model -> Html Msg
viewTeamHistory model =
    case model.teamHistory of
        RemoteData.Success history ->
            List.map (\h -> ( h.date, toFloat h.elo )) history
                |> LineChart.viewChart

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading History..." ]

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError
