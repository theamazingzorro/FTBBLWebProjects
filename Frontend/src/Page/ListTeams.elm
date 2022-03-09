module Page.ListTeams exposing (Model, Msg, init, update, view)

import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import RemoteData exposing (WebData)
import Team exposing (Team, teamsDecoder)
import Url exposing (Protocol(..))


type alias Model =
    { teams : WebData (List Team)
    }


type Msg
    = FetchTeams
    | TeamsReceived (WebData (List Team))


init : ( Model, Cmd Msg )
init =
    ( { teams = RemoteData.Loading }, getTeamsRequest )


getTeamsRequest : Cmd Msg
getTeamsRequest =
    Http.get
        { url = "https://localhost:17317/api/team"
        , expect =
            Http.expectJson (RemoteData.fromResult >> TeamsReceived) teamsDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchTeams ->
            ( { model | teams = RemoteData.Loading }, getTeamsRequest )

        TeamsReceived response ->
            ( { model | teams = response }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchTeams ]
            [ text "Refresh Teams" ]
        , viewTeamsOrError model
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
            viewError (Error.buildErrorMessage httpError)


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewTeams : List Team -> Html Msg
viewTeams teams =
    div []
        [ h3 [] [ text "Teams" ]
        , table []
            (viewTableHeader :: List.map viewTeam teams)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "Name" ]
        , th []
            [ text "Race" ]
        , th []
            [ text "Coach" ]
        ]


viewTeam : Team -> Html Msg
viewTeam team =
    tr []
        [ td []
            [ text team.name ]
        , td []
            [ text team.race.name ]
        , td []
            [ text team.coach ]
        ]
