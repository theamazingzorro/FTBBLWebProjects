module Page.ListAccolades exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Http
import Model.Accolade as Accolade exposing (Accolade, AccoladeId, accoladesDecoder)
import Model.Coach exposing (Coach, CoachId, coachsDecoder)
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Session exposing (Session)
import Model.SharedIds exposing (defaultTeamId)
import Model.Team exposing (Team, TeamId, teamsDecoder)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { accolades : WebData (List Accolade)
    , teams : WebData (List Team)
    , coaches : WebData (List Coach)
    , session : Session
    , deleteError : Maybe String
    }


type Msg
    = Refresh
    | AccoladesRecieved (WebData (List Accolade))
    | CoachesRecieved (WebData (List Coach))
    | TeamsRecieved (WebData (List Team))
    | AddAccoladeButtonClick
    | DeleteAccoladeButtonClick AccoladeId
    | AccoladeDeleted (Result Http.Error DeleteResponse)



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { accolades = RemoteData.Loading
      , coaches = RemoteData.Loading
      , teams = RemoteData.Loading
      , session = session
      , deleteError = Nothing
      }
    , Cmd.batch
        [ getAccoladesRequest session.token
        , getTeamsRequest session.token
        , getCoachesRequest session.token
        ]
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( { model | accolades = RemoteData.Loading, teams = RemoteData.Loading, coaches = RemoteData.Loading }
            , Cmd.batch
                [ getAccoladesRequest model.session.token
                , getTeamsRequest model.session.token
                , getCoachesRequest model.session.token
                ]
            )

        AccoladesRecieved response ->
            ( { model | accolades = response }, Cmd.none )

        CoachesRecieved response ->
            ( { model | coaches = response }, Cmd.none )

        TeamsRecieved response ->
            ( { model | teams = response }, Cmd.none )

        AddAccoladeButtonClick ->
            ( model, pushUrl model.session.navkey Route.AddAccolade )

        DeleteAccoladeButtonClick id ->
            ( model, deleteAccoladeRequest model.session.token id )

        AccoladeDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getAccoladesRequest model.session.token )

        AccoladeDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. Accolades cannot be deleted before their last team."


getTeam : WebData (List Team) -> Maybe TeamId -> Maybe Team
getTeam teamData teamId =
    case teamData of
        Success teams ->
            List.filter (\team -> team.id == Maybe.withDefault defaultTeamId teamId) teams
                |> List.head

        _ ->
            Nothing


getCoach : WebData (List Coach) -> CoachId -> Maybe Coach
getCoach coachData coachId =
    case coachData of
        Success coaches ->
            List.filter (\team -> team.id == coachId) coaches
                |> List.head

        _ ->
            Nothing



-- API Requests --


getAccoladesRequest : Maybe String -> Cmd Msg
getAccoladesRequest token =
    Api.getRequest token Api.Accolades <|
        Http.expectJson (RemoteData.fromResult >> AccoladesRecieved) accoladesDecoder


getCoachesRequest : Maybe String -> Cmd Msg
getCoachesRequest token =
    Api.getRequest token Api.Coaches <|
        Http.expectJson (RemoteData.fromResult >> CoachesRecieved) coachsDecoder


getTeamsRequest : Maybe String -> Cmd Msg
getTeamsRequest token =
    Api.getRequest token Api.Teams <|
        Http.expectJson (RemoteData.fromResult >> TeamsRecieved) teamsDecoder


deleteAccoladeRequest : Maybe String -> AccoladeId -> Cmd Msg
deleteAccoladeRequest token id =
    Api.deleteRequest token (Api.Accolade id) <|
        Http.expectJson AccoladeDeleted deleteResponseDecoder



-- View --


view : Model -> Html Msg
view model =
    row []
        [ viewRefreshButton
        , viewErrorMessage model.deleteError
        , viewAccoladesOrError model
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    optionButton [ onClick Refresh, floatRight ] [ text "Refresh Accolades" ]


viewAccoladesOrError : Model -> Html Msg
viewAccoladesOrError model =
    case model.accolades of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success accolades ->
            viewAccolades model accolades

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewLoadError : String -> Html Msg
viewLoadError errorMessage =
    errorText []
        [ emphasisText [] [ text "Couldn't fetch data at this time." ]
        , text <| "Error: " ++ errorMessage
        ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage message =
    case message of
        Just m ->
            errorText [] [ text <| "Error: " ++ m ]

        Nothing ->
            text ""


viewAccolades : Model -> List Accolade -> Html Msg
viewAccolades model accolades =
    div []
        [ viewHeader model.session
        , table []
            [ viewTableHeader
            , tableBody [] <|
                List.map (viewAccolade model) accolades
            ]
        ]


viewHeader : Session -> Html Msg
viewHeader session =
    row []
        [ mainHeader [] [ text "Accolades" ]
        , requiresAuth session viewAddButton
        ]


viewAddButton : Html Msg
viewAddButton =
    addButton
        [ onClick AddAccoladeButtonClick, floatRight ]
        [ text "Add Accolade" ]


viewTableHeader : Html Msg
viewTableHeader =
    tableHead []
        [ ( [], [ text "Team" ] )
        , ( [], [ text "Coach" ] )
        , ( [], [ text "Display" ] )
        , ( [], [ text "Name" ] )
        , ( [], [ text "Season" ] )
        , ( [], [ text "" ] )
        ]


viewAccolade : Model -> Accolade -> Html Msg
viewAccolade model accolade =
    tableRow []
        [ ( [], [ text (getTeam model.teams accolade.teamId |> Maybe.andThen (.name >> Just) |> Maybe.withDefault "") ] )
        , ( [], [ text (getCoach model.coaches accolade.coachId |> Maybe.andThen (.name >> Just) |> Maybe.withDefault "") ] )
        , ( [], [ Accolade.viewAccolade accolade ] )
        , ( [], [ text accolade.name ] )
        , ( [], [ text <| Maybe.withDefault "" <| Maybe.andThen (String.fromInt >> Just) accolade.season ] )
        , ( [], [ requiresAuth model.session <| viewDeleteButton accolade ] )
        ]


viewDeleteButton : Accolade -> Html Msg
viewDeleteButton accolade =
    warnButton
        [ onClick (DeleteAccoladeButtonClick accolade.id) ]
        [ text "Delete" ]
