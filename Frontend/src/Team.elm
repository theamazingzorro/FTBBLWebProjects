module Team exposing (..)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Race exposing (Race, raceDecoder)


type alias Team =
    { id : TeamId
    , name : String
    , race : Race
    , coach : String
    }


type alias TeamId =
    Int


teamIdFromInt : Int -> TeamId
teamIdFromInt i = i 


teamIdToInt : TeamId -> Int
teamIdToInt i = i 


teamsDecoder : Decoder (List Team)
teamsDecoder =
    list teamDecoder


teamDecoder : Decoder Team
teamDecoder =
    Decode.succeed Team
        |> required "id" teamIdDecoder
        |> required "name" string
        |> required "race" raceDecoder
        |> required "coach" string


teamIdDecoder : Decoder TeamId
teamIdDecoder =
    Decode.map teamIdFromInt int
