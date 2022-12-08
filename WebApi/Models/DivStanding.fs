namespace ftbbl.WebApi.Models

open System

[<CLIMutable>]
type DivStanding =
    {
        Div : Division

        TeamId : int

        Rank : int

        Wins : int
        Draws : int
        Losses : int
        PointsScored : int
        PointsGiven : int
    }
