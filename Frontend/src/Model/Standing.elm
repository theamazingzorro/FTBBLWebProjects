module Model.Standing exposing
    ( Standing
    , standingDecoder
    , standingsDecoder
    )

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Model.Division exposing (Division, divisionDecoder)
import Model.Division exposing (DivisionId)
import Model.Team exposing (Team)
import Model.Division exposing (divisionIdDecoder)
import Model.Team exposing (teamDecoder)



-- Types --


type alias Standing =
    { divId : DivisionId
    , team : Team
    , wins : Int
    , draws : Int
    , losses : Int
    , pointsScored : Int
    , pointsGiven : Int
    }



-- Decoders --


standingsDecoder : Decoder (List Standing)
standingsDecoder =
    list standingDecoder


standingDecoder : Decoder Standing
standingDecoder =
    Decode.succeed Standing
        |> required "divId" divisionIdDecoder
        |> required "team" teamDecoder
        |> required "wins" int
        |> required "draws" int
        |> required "losses" int
        |> required "pointsScored" int
        |> required "pointsGiven" int


