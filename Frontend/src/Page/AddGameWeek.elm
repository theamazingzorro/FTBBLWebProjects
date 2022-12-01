module Page.AddGameWeek exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Division exposing (Division, DivisionId, divisionDecoder)
import Model.Game exposing (Game, defaultGame)
import Model.Session exposing (Session)
import Model.Team as Team exposing (Team, defaultTeam, teamsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)
import Model.Game exposing (newGameEncoder)
import Model.Game exposing (gameDecoder)



-- Types --


type alias Model =
    { session : Session
    , divId : DivisionId
    , division : WebData Division
    , teams : WebData (List Team)
    , games : List Game
    , week : Int
    , submitErrors : List String
    , successes : Int
    }


type Msg
    = DivisionRecieved (WebData Division)
    | TeamsRecieved (WebData (List Team))
    | HomeTeamSelected Int String
    | AwayTeamSelected Int String
    | Submit
    | GameSubmitted (Result Http.Error Game)



-- Init --


init : Session -> DivisionId -> Int -> ( Model, Cmd Msg )
init session divId week =
    ( { session = session
      , divId = divId
      , division = RemoteData.Loading
      , teams = RemoteData.Loading
      , games = []
      , week = week
      , submitErrors = []
      , successes = 0
      }
    , Cmd.batch
        [ getDivisionRequest session.token divId
        , getTeamsRequest session.token divId
        ]
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DivisionRecieved division ->
            ( { model | division = division, games = tryCreateGames model.week division model.teams }, Cmd.none )

        TeamsRecieved teams ->
            ( { model | teams = teams, games = tryCreateGames model.week model.division teams }, Cmd.none )

        HomeTeamSelected gameIndex teamId ->
            let
                newTeam =
                    searchByIdString teamId Team.idToString defaultTeam model.teams
            in
            ( { model | games = changeHomeTeam gameIndex newTeam model.games }, Cmd.none )

        AwayTeamSelected gameIndex teamId ->
            let
                newTeam =
                    searchByIdString teamId Team.idToString defaultTeam model.teams
            in
            ( { model | games = changeAwayTeam gameIndex newTeam model.games }, Cmd.none )

        Submit ->
            ( { model | submitErrors = []}, saveGames model.session.token model.games)

        GameSubmitted (Ok _) ->
            let
                newSuccesses =
                    model.successes + 1

                newCmd =
                    if newSuccesses == List.length model.games then
                        pushUrl model.session.navkey <| Route.ViewDivision model.divId

                    else
                        Cmd.none
            in
            ( { model | successes = newSuccesses }, newCmd )

        GameSubmitted (Err err) ->
            ( { model | submitErrors = (buildErrorMessage err) :: model.submitErrors }, Cmd.none )


searchByIdString : String -> (id -> String) -> { c | id : id } -> WebData (List { c | id : id }) -> { c | id : id }
searchByIdString idString stringFromId defaultVal listData =
    case listData of
        RemoteData.Success list ->
            List.filter (\item -> stringFromId item.id == idString) list
                |> List.head
                |> Maybe.withDefault defaultVal

        _ ->
            defaultVal


changeHomeTeam : Int -> Team -> List Game -> List Game
changeHomeTeam gameIndex newTeam games =
    let
        changeIf game index =
            if index == gameIndex then
                { game | homeTeam = newTeam }

            else
                game
    in
    List.map2 changeIf games <| List.range 0 <| List.length games


changeAwayTeam : Int -> Team -> List Game -> List Game
changeAwayTeam gameIndex newTeam games =
    let
        changeIf game index =
            if index == gameIndex then
                { game | awayTeam = newTeam }

            else
                game
    in
    List.map2 changeIf games <| List.range 0 <| List.length games


tryCreateGames : Int -> WebData Division -> WebData (List Team) -> List Game
tryCreateGames week divData teamsData =
    case divData of
        RemoteData.Success div ->
            case teamsData of
                RemoteData.Success teams ->
                    let
                        gameCount =
                            List.length teams // 2
                    in
                    List.repeat gameCount { defaultGame | division = div, week=week }

                _ ->
                    []

        _ ->
            []



-- API Requests --


getDivisionRequest : Maybe String -> DivisionId -> Cmd Msg
getDivisionRequest token divId =
    Api.getRequest token (Api.Division divId) <|
        Http.expectJson (RemoteData.fromResult >> DivisionRecieved) divisionDecoder


getTeamsRequest : Maybe String -> DivisionId -> Cmd Msg
getTeamsRequest token divId =
    Api.getRequest token (Api.TeamsInDiv divId) <|
        Http.expectJson (RemoteData.fromResult >> TeamsRecieved) teamsDecoder


saveGames : Maybe String -> List Game -> Cmd Msg
saveGames token games =
    Cmd.batch <| List.map (saveGame token) games


saveGame : Maybe String -> Game -> Cmd Msg
saveGame token game =
    Api.postRequest token
        Api.Games
        (Http.jsonBody (newGameEncoder game))
        (Http.expectJson GameSubmitted gameDecoder)

-- View --


view : Model -> Html Msg
view model =
    case model.division of
        RemoteData.Success division ->
            div []
                [ h3 [] [ text <| division.name ++ " Season " ++ String.fromInt division.season ]
                , h5 [] [ text <| "Week " ++ String.fromInt model.week ]
                , br [] []
                , viewSubmitErrors model.submitErrors
                , viewForm model
                ]

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h4 [] [ text "Loading..." ]

        RemoteData.Failure httpError ->
            h4 [ Custom.Attributes.errorMessage ]
                [ text <| "Cannot load Division. " ++ Error.buildErrorMessage httpError ]


viewSubmitErrors : List String -> Html msg
viewSubmitErrors errors =
    if List.length errors > 0 then
        div [ Custom.Attributes.errorMessage ]
            [ h3 [] [ text "Couldn't save a team at this time." ]
            , text <| "Error: " ++ (List.foldl (\a b -> a ++ " " ++ b) "" errors )
            , br [] []
            ]
    else 
    text ""


viewForm : Model -> Html Msg
viewForm model =
    div [] <|
        List.append
            (List.map2 (viewGame model) model.games <| List.range 0 <| List.length model.games)
            [ submitButton ]


submitButton : Html Msg
submitButton =
    button
        [ Custom.Attributes.submitButton
        , onClick Submit
        ]
        [ text "Add Games" ]


viewGame : Model -> Game -> Int -> Html Msg
viewGame model game gameIndex =
    div []
        [ div Custom.Attributes.row
            [ viewDropdown game model.teams <| homeTeamDropdown gameIndex
            , text " vs. "
            , viewDropdown game model.teams <| awayTeamDropdown gameIndex
            ]
        , br [] []
        ]


viewDropdown : Game -> WebData (List { a | name : comparable }) -> (Game -> List { a | name : comparable } -> Html Msg) -> Html Msg
viewDropdown game data dropDownFunction =
    case data of
        RemoteData.NotAsked ->
            dropDownFunction game []

        RemoteData.Loading ->
            h4 [] [ text "Loading Options..." ]

        RemoteData.Failure httpError ->
            h4 [ Custom.Attributes.errorMessage ]
                [ text <| "Cannot load Options. " ++ Error.buildErrorMessage httpError ]

        RemoteData.Success d ->
            dropDownFunction game <| List.sortBy .name d


homeTeamDropdown : Int -> Game -> List Team -> Html Msg
homeTeamDropdown gameIndex game teams =
    div [ Custom.Attributes.col ]
        [ select
            (Custom.Attributes.formDropdown "homeTeamDropdown"
                [ onInput <| HomeTeamSelected gameIndex ]
            )
            (defaultOption :: (List.map (homeTeamOption game) <| List.filter (\team -> team.id /= game.awayTeam.id) teams))
        ]


homeTeamOption : Game -> Team -> Html msg
homeTeamOption game team =
    option
        [ value <| Team.idToString team.id
        , selected (team.id == game.homeTeam.id)
        ]
        [ text team.name ]


awayTeamDropdown : Int -> Game -> List Team -> Html Msg
awayTeamDropdown gameIndex game teams =
    div [ Custom.Attributes.col ]
        [ select
            (Custom.Attributes.formDropdown "awayTeamDropdown"
                [ onInput <| AwayTeamSelected gameIndex ]
            )
            (defaultOption :: (List.map (awayTeamOption game) <| List.filter (\team -> team.id /= game.homeTeam.id) teams))
        ]


awayTeamOption : Game -> Team -> Html msg
awayTeamOption game team =
    option
        [ value <| Team.idToString team.id
        , selected (team.id == game.awayTeam.id)
        ]
        [ text team.name ]


defaultOption : Html Msg
defaultOption =
    option [ value "0" ] [ text "-" ]
