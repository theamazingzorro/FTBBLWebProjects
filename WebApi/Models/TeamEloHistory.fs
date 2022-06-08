namespace ftbbl.WebApi.Models

open System
open NPoco

[<CLIMutable>]
type TeamEloHistory =
    {
        Id : int
        [<Column(Name = "team_id")>]
        TeamId : int
        Elo : int
        Date : DateTime
    }

