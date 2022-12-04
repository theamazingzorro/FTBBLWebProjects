namespace ftbbl.WebApi.Models

[<CLIMutable>]
type Standing =
    {
        DivId : int

        Team : Team

        Wins : int
        Draws : int
        Losses : int
        PointsScored : int
        PointsGiven : int
    }
