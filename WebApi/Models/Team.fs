namespace ftbbl.WebApi.Models

open NPoco

[<CLIMutable>]
type Team =
    {
        Id : int
        Name : string
        [<Column("race_id")>]
        Race: int
        Coach: string
    }
