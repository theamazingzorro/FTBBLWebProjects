namespace ftbbl.WebApi.Models

open System
open NPoco

[<CLIMutable>]
type CoachEloHistory =
    {
        Id : int
        [<Column(Name = "coach_id")>]
        CoachId : int
        Elo : int
        Date : DateTime
    }

