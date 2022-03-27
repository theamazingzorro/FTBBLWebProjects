module Model.Team exposing (Team, TeamId, defaultTeam, idParser, idToString, teamsDecoder)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Model.Coach exposing (Coach, coachDecoder, defaultCoach)
import Model.Race exposing (Race, defaultRace, raceDecoder)
import Url.Parser exposing (Parser, custom)



-- Types --


type alias Team =
    { id : TeamId
    , name : String
    , race : Race
    , coach : Coach
    , elo : Int
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


teamIdDecoder : Decoder TeamId
teamIdDecoder =
    Decode.map TeamId int



-- Parsers --


idParser : Parser (TeamId -> a) a
idParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map TeamId (String.toInt postId)
