module Page.AddGame exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Division as Division exposing (Division, DivisionId, divisionsDecoder)
import Model.Game exposing (Game, defaultGame, gameDecoder, newGameEncoder)
import Model.Session exposing (Session)
import Model.Team as Team exposing (Team, defaultTeam, teamsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { session : Session
    , game : Game
    , defaultDivId : String
    , divisionOptions : WebData (List Division)
    , teamOptions : WebData (List Team)
    , saveError : Maybe String
    }


type Msg
    = DivisionsRecieved (WebData (List Division))
    | TeamsRecieved (WebData (List Team))
    | Submit
    | GameSubmitted (Result Http.Error Game)
    | WeekChanged String
    | HomeScoreChanged String
    | AwayScoreChanged String
    | DivisionSelected String
    | HomeTeamSelected String
    | AwayTeamSelected String



-- Init --


init : Session -> Maybe DivisionId -> Maybe Int -> ( Model, Cmd Msg )
init session divId week =
    ( { session = session
      , game = { defaultGame | week = Maybe.withDefault 0 week }
      , defaultDivId =
            Maybe.andThen (Division.idToString >> Just) divId
                |> Maybe.withDefault ""
      , divisionOptions = RemoteData.Loading
      , teamOptions = RemoteData.NotAsked
      , saveError = Nothing
      }
    , getDivisionsRequest session.token
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DivisionsRecieved (RemoteData.Success divisions) ->
            let
                openDivs =
                    List.filter (\div -> not div.closed) divisions

                newGame =
                    reDiv model.game (RemoteData.Success openDivs) model.defaultDivId
            in
            ( { model | divisionOptions = RemoteData.Success openDivs, game = newGame }
            , getTeamsRequest model.session.token newGame.division.id
            )

        DivisionsRecieved divisions ->
            ( { model | divisionOptions = divisions }, Cmd.none )

        TeamsRecieved teams ->
            ( { model | teamOptions = teams }, Cmd.none )

        Submit ->
            ( { model | saveError = Nothing }, saveGame model.session.token model.game )

        GameSubmitted (Ok _) ->
            ( { model | saveError = Nothing }, pushUrl model.session.navkey <| Route.ViewDivision model.game.division.id )

        GameSubmitted (Err err) ->
            ( { model | saveError = Just (buildErrorMessage err) }, Cmd.none )

        WeekChanged newWeek ->
            let
                reWeek game week =
                    { game | week = String.toInt week |> Maybe.withDefault 0 }
            in
            ( { model | game = reWeek model.game newWeek }, Cmd.none )

        HomeScoreChanged newScore ->
            let
                reHomeScore game score =
                    { game | homeScore = String.toInt score }
            in
            ( { model | game = reHomeScore model.game newScore }, Cmd.none )

        AwayScoreChanged newScore ->
            let
                reAwayScore game score =
                    { game | awayScore = String.toInt score }
            in
            ( { model | game = reAwayScore model.game newScore }, Cmd.none )

        DivisionSelected newDiv ->
            let
                newGame =
                    reDiv model.game model.divisionOptions newDiv
            in
            ( { model | game = newGame, teamOptions = RemoteData.Loading }
            , getTeamsRequest model.session.token newGame.division.id
            )

        HomeTeamSelected teamId ->
            let
                reteam game =
                    { game | homeTeam = getTeamForIdString model.teamOptions teamId }
            in
            ( { model | game = reteam model.game }, Cmd.none )

        AwayTeamSelected teamId ->
            let
                reteam game =
                    { game | awayTeam = getTeamForIdString model.teamOptions teamId }
            in
            ( { model | game = reteam model.game }, Cmd.none )


reDiv : Game -> WebData (List Division) -> String -> Game
reDiv game data divId =
    case data of
        RemoteData.Success divOptions ->
            let
                newDiv =
                    searchByIdString divId Division.idToString game.division divOptions
            in
            { game | division = newDiv, homeTeam = defaultTeam, awayTeam = defaultTeam }

        _ ->
            game


getTeamForIdString : WebData (List Team) -> String -> Team
getTeamForIdString data idString =
    case data of
        RemoteData.Success teams ->
            searchByIdString idString Team.idToString defaultTeam teams

        _ ->
            defaultTeam


searchByIdString : String -> (id -> String) -> { c | id : id } -> List { c | id : id } -> { c | id : id }
searchByIdString idString stringFromId defaultVal list =
    List.filter (\item -> stringFromId item.id == idString) list
        |> List.head
        |> Maybe.withDefault defaultVal



-- API Requests --


getDivisionsRequest : Maybe String -> Cmd Msg
getDivisionsRequest token =
    Api.getRequest token Api.Divisions <|
        Http.expectJson (RemoteData.fromResult >> DivisionsRecieved) divisionsDecoder


getTeamsRequest : Maybe String -> DivisionId -> Cmd Msg
getTeamsRequest token divId =
    Api.getRequest token (Api.TeamsInDiv divId) <|
        Http.expectJson (RemoteData.fromResult >> TeamsRecieved) teamsDecoder


saveGame : Maybe String -> Game -> Cmd Msg
saveGame token game =
    Api.postRequest token
        Api.Games
        (Http.jsonBody (newGameEncoder game))
        (Http.expectJson GameSubmitted gameDecoder)



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Add Game" ]
        , br [] []
        , viewSaveError model.saveError
        , viewGame model model.game
        ]


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a game at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


viewGame : Model -> Game -> Html Msg
viewGame model game =
    div []
        [ viewDropdown game model.divisionOptions divisionDropdown
        , viewWeekField game.week
        , viewDropdown game (filterWebData (\team -> team.id /= game.awayTeam.id) model.teamOptions) homeTeamDropdown
        , viewHomeScoreField game.homeScore
        , viewDropdown game (filterWebData (\team -> team.id /= game.homeTeam.id) model.teamOptions) awayTeamDropdown
        , viewAwayScoreField game.awayScore
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Save" ]
        ]


filterWebData : (a -> Bool) -> WebData (List a) -> WebData (List a)
filterWebData filterFunc =
    RemoteData.andThen (List.filter filterFunc >> RemoteData.Success)


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


divisionDropdown : Game -> List Division -> Html Msg
divisionDropdown game divs =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "divDropdown")
            [ text "Division" ]
        , select
            (Custom.Attributes.formDropdown "divDropdown"
                [ onInput DivisionSelected ]
            )
            (defaultOption :: List.map (divOption game) divs)
        ]


divOption : Game -> Division -> Html msg
divOption game division =
    option
        [ value <| Division.idToString division.id
        , selected (division.id == game.division.id)
        ]
        [ text <| division.name ++ " Season " ++ String.fromInt division.season ]


homeTeamDropdown : Game -> List Team -> Html Msg
homeTeamDropdown game teams =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "homeTeamDropdown")
            [ text "Home Team" ]
        , select
            (Custom.Attributes.formDropdown "homeTeamDropdown"
                [ onInput HomeTeamSelected ]
            )
            (defaultOption :: List.map (homeTeamOption game) teams)
        ]


homeTeamOption : Game -> Team -> Html msg
homeTeamOption game team =
    option
        [ value <| Team.idToString team.id
        , selected (team.id == game.homeTeam.id)
        ]
        [ text team.name ]


awayTeamDropdown : Game -> List Team -> Html Msg
awayTeamDropdown game teams =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "awayTeamDropdown")
            [ text "Away Team" ]
        , select
            (Custom.Attributes.formDropdown "awayTeamDropdown"
                [ onInput AwayTeamSelected ]
            )
            (defaultOption :: List.map (awayTeamOption game) teams)
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


viewWeekField : Int -> Html Msg
viewWeekField val =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "weekInput")
            [ text "Week" ]
        , input
            (Custom.Attributes.formInput "weekInput"
                [ onInput WeekChanged
                , value <| String.fromInt val
                ]
            )
            []
        ]


viewHomeScoreField : Maybe Int -> Html Msg
viewHomeScoreField val =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "homeScoreInput")
            [ text "Home Score" ]
        , input
            (Custom.Attributes.formInput "homeScoreInput"
                [ onInput HomeScoreChanged
                , value <| stringFromMaybeInt val
                ]
            )
            []
        ]


viewAwayScoreField : Maybe Int -> Html Msg
viewAwayScoreField val =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "awayScoreInput")
            [ text "Away Score" ]
        , input
            (Custom.Attributes.formInput "awayScoreInput"
                [ onInput AwayScoreChanged
                , onEnter Submit
                , value <| stringFromMaybeInt val
                ]
            )
            []
        ]


stringFromMaybeInt : Maybe Int -> String
stringFromMaybeInt i =
    Maybe.andThen (String.fromInt >> Just) i |> Maybe.withDefault ""
