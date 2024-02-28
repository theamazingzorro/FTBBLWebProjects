module Page.EditGame exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Game exposing (Game, GameId, gameDecoder, gameEncoder)
import Model.Session exposing (Session)
import RemoteData exposing (WebData)
import Route exposing (Route, pushUrl)



-- Types --


type alias Model =
    { session : Session
    , gameId : GameId
    , game : WebData Game
    , saveError : Maybe String
    }


type Msg
    = GameReceived (WebData Game)
    | Submit
    | GameSubmitted (Result Http.Error Game)
    | WeekChanged String
    | HomeScoreChanged String
    | AwayScoreChanged String



-- Init --


init : Session -> GameId -> ( Model, Cmd Msg )
init session gameId =
    ( { session = session
      , gameId = gameId
      , game = RemoteData.Loading
      , saveError = Nothing
      }
    , getGameRequest session.token gameId
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameReceived response ->
            ( { model | game = response }, Cmd.none )

        Submit ->
            trySaveGame model

        GameSubmitted (Ok _) ->
            ( { model | saveError = Nothing }, pushUrl model.session.navkey <| getNextPage model.game )

        GameSubmitted (Err err) ->
            ( { model | saveError = Just (buildErrorMessage err) }, Cmd.none )

        WeekChanged newWeek ->
            ( { model | game = reWeek model.game newWeek }, Cmd.none )

        HomeScoreChanged newScore ->
            ( { model | game = reHomeScore model.game newScore }, Cmd.none )

        AwayScoreChanged newScore ->
            ( { model | game = reAwayScore model.game newScore }, Cmd.none )


reWeek : WebData Game -> String -> WebData Game
reWeek data week =
    case data of
        RemoteData.Success game ->
            let
                newWeek =
                    String.toInt week
                        |> Maybe.withDefault 0
            in
            RemoteData.Success { game | week = newWeek }

        _ ->
            data


reHomeScore : WebData Game -> String -> WebData Game
reHomeScore data score =
    case data of
        RemoteData.Success game ->
            RemoteData.Success { game | homeScore = String.toInt score }

        _ ->
            data


reAwayScore : WebData Game -> String -> WebData Game
reAwayScore data score =
    case data of
        RemoteData.Success game ->
            RemoteData.Success { game | awayScore = String.toInt score }

        _ ->
            data


trySaveGame : Model -> ( Model, Cmd Msg )
trySaveGame model =
    case model.game of
        RemoteData.Success game ->
            ( { model | saveError = Nothing }, saveGame model.session.token game )

        _ ->
            ( { model | saveError = Just "Cannot submit data, please refresh page and try again." }, Cmd.none )


getNextPage : WebData Game -> Route
getNextPage data =
    case data of
        RemoteData.Success game ->
            Route.ViewDivisionWeek game.division.id game.week

        _ ->
            Route.Divisions



-- API Requests --


getGameRequest : Maybe String -> GameId -> Cmd Msg
getGameRequest token id =
    Api.getRequest token (Api.Game id) <|
        Http.expectJson (RemoteData.fromResult >> GameReceived) gameDecoder


saveGame : Maybe String -> Game -> Cmd Msg
saveGame token game =
    Api.putRequest token
        (Api.Game game.id)
        (Http.jsonBody (gameEncoder game))
    <|
        Http.expectJson GameSubmitted gameDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit Game" ]
        , br [] []
        , viewSaveError model.saveError
        , viewGameOrError model
        ]


viewGameOrError : Model -> Html Msg
viewGameOrError model =
    case model.game of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success game ->
            viewGame game

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


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


viewLoadError : String -> Html Msg
viewLoadError errorMessage =
    div [ Custom.Attributes.errorMessage ]
        [ h3 [] [ text "Couldn't fetch data at this time." ]
        , text <| "Error: " ++ errorMessage
        ]


viewGame : Game -> Html Msg
viewGame game =
    div []
        [ viewStaticField "divisionName" "Division" <| game.division.name ++ " Season " ++ String.fromInt game.division.season
        , viewWeekField game.week
        , viewStaticField "homeTeamName" "Home Team" game.homeTeam.name
        , viewHomeScoreField game.homeScore
        , viewStaticField "awayTeamName" "Away Team" game.awayTeam.name
        , viewAwayScoreField game.awayScore
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Save" ]
        ]


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
