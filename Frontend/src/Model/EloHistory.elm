module Model.EloHistory exposing (EloHistory, historyDecoder, historyListDecoder, maxElo)

import Json.Decode as Decode exposing (Decoder, int, list)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (required)
import Time



-- Types --


type alias EloHistory =
    { elo : Int
    , date : Time.Posix
    }



-- Utils --


maxElo : List EloHistory -> Int
maxElo list =
    case list of
        [] ->
            0

        [ h ] ->
            h.elo

        h :: tail ->
            let
                choose x y =
                    if x.elo > y.elo then
                        x

                    else
                        y
            in
            .elo <| List.foldl choose h tail



-- Decoders --


historyListDecoder : Decoder (List EloHistory)
historyListDecoder =
    list historyDecoder


historyDecoder : Decoder EloHistory
historyDecoder =
    Decode.succeed EloHistory
        |> required "elo" int
        |> required "date" datetime
