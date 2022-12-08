module Model.DivStanding exposing
    ( Standing
    , compareStandings
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


type alias Standing =
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


getPoints : Standing -> Int
getPoints standing =
    3 * standing.wins + standing.draws


getGamesPlayed : Standing -> Int
getGamesPlayed standing =
    standing.wins + standing.losses + standing.draws


getTDD : Standing -> Int
getTDD standing =
    standing.pointsScored - standing.pointsGiven


compareStandings : Standing -> Standing -> Order
compareStandings a b =
    compare a.rank b.rank



-- Decoders --


standingsDecoder : Decoder (List Standing)
standingsDecoder =
    list standingDecoder


standingDecoder : Decoder Standing
standingDecoder =
    Decode.succeed Standing
        |> required "divId" divisionDecoder
        |> required "team" teamIdDecoder
        |> required "rank" int
        |> required "wins" int
        |> required "draws" int
        |> required "losses" int
        |> required "pointsScored" int
        |> required "pointsGiven" int
