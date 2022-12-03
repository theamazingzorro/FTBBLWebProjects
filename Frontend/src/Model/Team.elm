module Model.Team exposing
    ( Team
    , TeamId
    , defaultTeam
    , idParser
    , idToString
    , newTeamEncoder
    , teamDecoder
    , teamEncoder
    , teamsDecoder
    )

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Model.Coach exposing (Coach, coachDecoder, coachEncoder, defaultCoach)
import Model.Division exposing (Division, divisionDecoder)
import Model.Race exposing (Race, defaultRace, raceDecoder, raceEncoder)
import Url.Parser exposing (Parser, custom)



-- Types --


type alias Team =
    { id : TeamId
    , name : String
    , race : Race
    , coach : Coach
    , elo : Int
    , division : Maybe Division
    }


type TeamId
    = TeamId Int



-- ToString --


idToString : TeamId -> String
idToString (TeamId id) =
    String.fromInt id



-- Default --


defaultTeam : Team
defaultTeam =
    { id = TeamId 0
    , name = ""
    , race = defaultRace
    , coach = defaultCoach
    , elo = 1000
    , division = Nothing
    }



-- Decoders --


teamsDecoder : Decoder (List Team)
teamsDecoder =
    list teamDecoder


teamDecoder : Decoder Team
teamDecoder =
    Decode.succeed Team
        |> required "id" teamIdDecoder
        |> required "name" string
        |> required "race" raceDecoder
        |> required "coach" coachDecoder
        |> required "elo" int
        |> optional "division" (Decode.map Just divisionDecoder) Nothing


teamIdDecoder : Decoder TeamId
teamIdDecoder =
    Decode.map TeamId int



-- Encoders --


teamEncoder : Team -> Encode.Value
teamEncoder team =
    Encode.object
        [ ( "id", encodeId team.id )
        , ( "name", Encode.string team.name )
        , ( "race", raceEncoder team.race )
        , ( "coach", coachEncoder team.coach )
        , ( "elo", Encode.int team.elo )
        ]


newTeamEncoder : Team -> Encode.Value
newTeamEncoder team =
    Encode.object
        [ ( "name", Encode.string team.name )
        , ( "race", raceEncoder team.race )
        , ( "coach", coachEncoder team.coach )
        , ( "elo", Encode.int team.elo )
        ]


encodeId : TeamId -> Encode.Value
encodeId (TeamId id) =
    Encode.int id



-- Parsers --


idParser : Parser (TeamId -> a) a
idParser =
    custom "TEAMID" <|
        \id ->
            Maybe.map TeamId (String.toInt id)
