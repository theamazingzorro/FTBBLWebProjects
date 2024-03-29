module Model.Game exposing
    ( Game
    , GameId
    , defaultGame
    , gameDecoder
    , gameEncoder
    , gamesDecoder
    , idParser
    , idToString
    , newGameEncoder
    , oddsToString
    )

import Html.Attributes exposing (id)
import Json.Decode as Decode exposing (Decoder, float, int, list)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Model.Division exposing (Division, defaultDivision, divisionDecoder, divisionEncoder)
import Model.Team exposing (Team, defaultTeam, teamDecoder, teamEncoder)
import Url.Parser exposing (Parser, custom)



-- Types --


type alias Game =
    { id : GameId
    , homeScore : Maybe Int
    , awayScore : Maybe Int
    , homeOdds : Maybe Float
    , awayOdds : Maybe Float
    , week : Int
    , division : Division
    , homeTeam : Team
    , awayTeam : Team
    }


type GameId
    = GameId Int



-- ToString --


idToString : GameId -> String
idToString (GameId id) =
    String.fromInt id


oddsToString : Float -> String
oddsToString odds =
    let
        trucOdds =
            toFloat (truncate (odds * 100)) / 100
    in
    String.fromFloat trucOdds ++ "%"



--Default--


defaultGame : Game
defaultGame =
    { id = GameId 0
    , homeScore = Nothing
    , awayScore = Nothing
    , homeOdds = Nothing
    , awayOdds = Nothing
    , week = 0
    , division = defaultDivision
    , homeTeam = defaultTeam
    , awayTeam = defaultTeam
    }



-- Decoders --


gamesDecoder : Decoder (List Game)
gamesDecoder =
    list gameDecoder


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> required "id" gameIdDecoder
        |> optional "homeScore" (Decode.map Just int) Nothing
        |> optional "awayScore" (Decode.map Just int) Nothing
        |> optional "homeOdds" (Decode.map Just float) Nothing
        |> optional "awayOdds" (Decode.map Just float) Nothing
        |> required "week" int
        |> required "division" divisionDecoder
        |> required "homeTeam" teamDecoder
        |> required "awayTeam" teamDecoder


gameIdDecoder : Decoder GameId
gameIdDecoder =
    Decode.map GameId int



-- Encoders --


gameEncoder : Game -> Encode.Value
gameEncoder game =
    Encode.object
        [ ( "id", encodeId game.id )
        , ( "awayScore", encodeMaybeInt game.awayScore )
        , ( "homeScore", encodeMaybeInt game.homeScore )
        , ( "week", Encode.int game.week )
        , ( "awayTeam", teamEncoder game.awayTeam )
        , ( "homeTeam", teamEncoder game.homeTeam )
        , ( "division", divisionEncoder game.division )
        ]


newGameEncoder : Game -> Encode.Value
newGameEncoder game =
    Encode.object
        [ ( "week", Encode.int game.week )
        , ( "awayTeam", teamEncoder game.awayTeam )
        , ( "homeTeam", teamEncoder game.homeTeam )
        , ( "division", divisionEncoder game.division )
        ]


encodeMaybeInt : Maybe Int -> Encode.Value
encodeMaybeInt maybeInt =
    maybeInt
        |> Maybe.map Encode.int
        |> Maybe.withDefault Encode.null


encodeId : GameId -> Encode.Value
encodeId (GameId id) =
    Encode.int id



-- Parsers --


idParser : Parser (GameId -> a) a
idParser =
    custom "GAMEID" <|
        \id ->
            Maybe.map GameId (String.toInt id)
