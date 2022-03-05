namespace ftbbl.WebApi.Models

[<CLIMutable>]
type Team =
    {
        Name : string
        Race: string
        Coach: string
        IsActive: bool
    }
