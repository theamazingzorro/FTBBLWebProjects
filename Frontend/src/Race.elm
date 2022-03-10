module Race exposing (..)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)


type alias Race =
    { id : RaceId
    , name : String
    }


type alias RaceId =
    Int


raceIdFromInt : Int -> RaceId
raceIdFromInt i =
    i


raceIdToInt : RaceId -> Int
raceIdToInt i =
    i


raceDecoder : Decoder Race
raceDecoder =
    Decode.succeed Race
        |> required "id" raceIdDecoder
        |> required "name" string


raceIdDecoder : Decoder RaceId
raceIdDecoder =
    Decode.map raceIdFromInt int
