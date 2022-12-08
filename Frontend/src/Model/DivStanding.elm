module Model.DivStanding exposing
    ( DivStanding
    , getGamesPlayed
    , getPoints
    , getTDD
    , standingDecoder
    , standingsDecoder
    )

import Json.Decode as Decode exposing (Decoder, int, list)
import Json.Decode.Pipeline exposing (required)
import Model.Division exposing (Division, divisionDecoder)
import Model.Team exposing (TeamId, teamIdDecoder)



-- Types --


type alias DivStanding =
    { divId : Division
    , team : TeamId
    , rank : Int
    , wins : Int
    , draws : Int
    , losses : Int
    , pointsScored : Int
    , pointsGiven : Int
    }



-- Utils --


getPoints : DivStanding -> Int
getPoints standing =
    3 * standing.wins + standing.draws


getGamesPlayed : DivStanding -> Int
getGamesPlayed standing =
    standing.wins + standing.losses + standing.draws


getTDD : DivStanding -> Int
getTDD standing =
    standing.pointsScored - standing.pointsGiven



-- Decoders --


standingsDecoder : Decoder (List DivStanding)
standingsDecoder =
    list standingDecoder


standingDecoder : Decoder DivStanding
standingDecoder =
    Decode.succeed DivStanding
        |> required "divId" divisionDecoder
        |> required "team" teamIdDecoder
        |> required "rank" int
        |> required "wins" int
        |> required "draws" int
        |> required "losses" int
        |> required "pointsScored" int
        |> required "pointsGiven" int
