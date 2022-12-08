module Model.EloHistory exposing (EloHistory, historyDecoder, historyListDecoder)

import Json.Decode as Decode exposing (Decoder, int, list)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (required)
import Time



-- Types --


type alias EloHistory =
    { elo : Int
    , date : Time.Posix
    }



-- Decoders --


historyListDecoder : Decoder (List EloHistory)
historyListDecoder =
    list historyDecoder


historyDecoder : Decoder EloHistory
historyDecoder =
    Decode.succeed EloHistory
        |> required "elo" int
        |> required "date" datetime
