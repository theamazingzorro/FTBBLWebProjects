namespace ftbbl.WebApi.Models

open NPoco

[<CLIMutable>]
type Team =
    {
        Name : string
        [<Column("race_id")>]
        Race: int
        Coach: string
    }
