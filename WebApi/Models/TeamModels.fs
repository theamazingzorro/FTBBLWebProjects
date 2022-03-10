namespace ftbbl.WebApi.Models.Team

[<CLIMutable>]
type Race =
    {
        Id : int
        Name : string
    }

[<CLIMutable>]
type Team =
    {
        Id : int
        Name : string
        Race : Race
        Coach : Coach
        Elo : int
    }

and [<CLIMutable>]
Coach =
    {
        Id : int
        Name : string
        Elo : int
    }


