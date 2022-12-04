module Model.Standing exposing
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
import Model.Division exposing (DivisionId, divisionIdDecoder)
import Model.Team exposing (Team, teamDecoder)



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
    case compare (getPoints a) (getPoints b) of
        EQ ->
            compare (getTDD a) (getTDD b)

        other ->
            other



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
