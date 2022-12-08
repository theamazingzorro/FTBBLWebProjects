namespace ftbbl.WebApi.Models

open System

[<CLIMutable>]
type Standing =
    {
        DivId : int

        Team : Team

        Rank : int

        Wins : int
        Draws : int
        Losses : int
        PointsScored : int
        PointsGiven : int

        AvgRemainingElo : Nullable<int>
    }
